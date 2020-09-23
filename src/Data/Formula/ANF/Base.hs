{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Formula.ANF.Base where

import           Control.DeepSeq                ( NFData )
import           Data.Either                    ( rights )
import qualified Data.Map.Strict               as M

import qualified Data.Formula.Prop             as P
import           Data.SAT.DIMACS
import           Data.SAT                       ( IsSAT(..)
                                                , SAT(..)
                                                )

import           Smtlib.Syntax.Syntax
import           Text.PrettyPrint.Leijen        ( (<+>)
                                                , Doc
                                                , Pretty
                                                , pretty
                                                , text
                                                )

import           GHC.Generics

-- SAT formula in ANF notation with Int variables that can be parsed
-- from a String
type BaseSAT = SAT ANF Int

baseSat :: BaseSAT
baseSat = SAT { solveSAT      = solve
              , minimize      = minimizeBase
              , solveSolution = const []
              , parseFormula  = parseBaseANF
              }

{-| ANF data type can construct an infinite tree of expressions. It can derive
  from both branches and could not be of canonical form. For a 'canonicalish'
  approach, see @CANF@.
-}
data ANF a
  = XOr (ANF a) (ANF a)
  | And (ANF a) (ANF a)
  | Var a
  | T
  | F
  deriving (Show, Eq, Ord, Generic, NFData, Functor, Foldable, Traversable)

instance Pretty a => Pretty (ANF a) where
  pretty = prettyBase

data CANF a = CXOr (CAnd a) (CANF a)
data CAnd a = CValue a (CAnd a) | Single (CValue a)
data CValue a = CVar a | CLit Bool

prettyBase :: Pretty a => ANF a -> Doc
prettyBase (And l@(XOr _ _) r@(XOr _ _)) =
  text "(" <> pretty l <> text ")(" <> pretty r <> text ")"
prettyBase (And l@(XOr _ _) r          ) = text "(" <> pretty l <> text ")" <> pretty r
prettyBase (And l           r@(XOr _ _)) = pretty l <> text "(" <> pretty r <> text ")"
prettyBase (And l           r          ) = pretty l <> pretty r
prettyBase (XOr l           r          ) = pretty l <+> (text "⊕" <+> pretty r)
prettyBase (Var a                      ) = pretty a
prettyBase T                             = text "1"
prettyBase F                             = text "0"

parseBaseANF :: DIMACS Int -> ANF Int
parseBaseANF = toXOr . clauses

-- | @Clause@ = @[Int]@, so this function simply build a tree of left associated
-- variables.
--
-- >>> toAnd [1,2,3] == And (And (Var 1) (Var 2)) (Var 3)
toAnd :: Clause -> ANF Int
toAnd []  = T
toAnd cls = foldl1 And . fmap Var $ cls

toXOr :: [Clause] -> ANF Int
toXOr []  = F -- This is valid :D (false is the z over xor)
toXOr cls = foldl1 XOr . map toAnd $ cls

---------------- Solver ------------------

-- |
--
--  We want to check if there is a combination of variable that satisfy
--  this formula. This function will return either SAT or UNSAT.
--
--  The method to achieve the answer will be to keep track of the number
--  of monomial occurrences in an ANF formula. If it doesn't collapse
--  when simplifying repeated formulas, the answer is SAT.
--
--  We are going to store this formulas as a set of pairs
--   - (formula, number of ocurrences)
--  Working in modulo 2 arithmetic to count them.
xor :: Bool -> Bool -> Bool
True  `xor` True  = False
True  `xor` False = True
False `xor` True  = True
False `xor` False = False

infixr 4 `xor`

type Record = M.Map (ANF Int) Int

solve :: ANF Int -> IsSAT
solve anf = if solve' anf > 0 then Satisfiable else Unsatisfiable
 where
  solve' :: ANF Int -> Int
  solve' f = M.size . M.filter odd $ build M.empty f

  build :: Record -> ANF Int -> Record
  build m (XOr l r) = m `merge` build M.empty l `merge` build M.empty r
  build m other     = M.insertWith (+) other 1 m

  merge :: Record -> Record -> Record
  merge = M.unionWith (+)

----------------

{-| Parses a subset of an Smtlib expression.

-}
smtToBaseANF :: Source -> [ANF String]
smtToBaseANF = rights . map commandToANF
 where

  commandToANF :: Command -> Either String (ANF String)
  commandToANF (Assert term) = Right $ termToANF term
  commandToANF _             = Left "Command was not an Assert"

  termToANF :: Term -> ANF String
  termToANF (TermQualIdentifier (QIdentifier (ISymbol "true" ))) = T
  termToANF (TermQualIdentifier (QIdentifier (ISymbol "false"))) = F
  termToANF (TermQualIdentifier (QIdentifier (ISymbol sym    ))) = Var sym
  termToANF (TermQualIdentifierT (QIdentifier (ISymbol "xor")) terms) =
    foldl1 XOr (map termToANF terms)
  termToANF (TermQualIdentifierT (QIdentifier (ISymbol "and")) terms) =
    foldl1 And (map termToANF terms)
  termToANF _ = F

---------------

-- | Canonical form of an ANF
--
-- Cases:
--   min a(b+c) => (min ab) + (min ac)
--   min (b+c)a => (min ab) + (min ac)
--   min a + b  => min a + min b
--   min aa     => min a
minimizeBase :: Eq a => ANF a -> ANF a
minimizeBase (And left (XOr r1 r2)) =
  minimizeBase $ XOr (minimizeBase (And left r1)) (minimizeBase (And left r2))
minimizeBase (And (XOr xleft xright) right) =
  minimizeBase $ XOr (minimizeBase (And xleft right)) (minimizeBase (And xright right))
minimizeBase (XOr (XOr l1 l2) right) =
  minimizeBase $ XOr (minimizeBase l1) (minimizeBase $ XOr l2 right)
minimizeBase (XOr left right)                 = XOr (minimizeBase left) (minimizeBase right)
minimizeBase (And left right) | left == right = minimizeBase left
minimizeBase anf                              = anf

height :: (Num a, Ord a) => ANF b -> a
height (XOr l r) = 1 + max (height l) (height r)
height (And l r) = 1 + max (height l) (height r)
height _         = 1

unXOr :: ANF a -> [ANF a]
unXOr (XOr l r) = l : (unXOr r)
unXOr anf       = [anf]

---------------

fromProp :: P.Prop a -> ANF a
fromProp (P.And l r) = And (fromProp l) (fromProp r)
fromProp (P.XOr l r) = XOr (fromProp l) (fromProp r)
fromProp (P.Or  l r) = XOr (And (fromProp l) (fromProp r)) (XOr (fromProp l) (fromProp r)) -- a || b = ab + a + b
fromProp (P.Imp l r) = XOr (And (fromProp l) (fromProp r)) (XOr (fromProp l) T) -- a => b = ab + a + 1
fromProp (P.Iff l r) = XOr (fromProp l) (XOr (fromProp r) T) -- a <=> b = a + b + 1
fromProp (P.Not p  ) = XOr (fromProp p) T -- not a = a + 1
fromProp (P.Var v  ) = Var v
fromProp (P.T      ) = T
fromProp (P.F      ) = F
