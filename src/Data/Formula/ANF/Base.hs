{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Formula.ANF.Base where

import           Control.DeepSeq                ( NFData )
import           Data.Either                    ( rights )
import qualified Data.Map.Strict               as M
import           Data.SAT                       ( IsSAT(..)
                                                , SAT(..)
                                                )
import           Data.SAT.DIMACS
import           GHC.Generics
import           Smtlib.Syntax.Syntax
import           Text.PrettyPrint.Leijen        ( (<+>)
                                                , Doc
                                                , Pretty
                                                , pretty
                                                , text
                                                )

-- SAT formula in ANF notation with Int variables that can be parsed
-- from a String
type BaseSAT = SAT ANF Int

baseSat :: BaseSAT
baseSat =
  SAT { solveSAT = solve, minimize = id -- minimizeBase,
                                       , solveSolution = const [], parseFormula = parseBaseANF }

{-| ANF data type can construct an infinite tree of expressions. It can derive
  from both branches and could not be of canonical form. For a 'canonicalish'
  approach, see @CANF@.
-}
data ANF a
  = XOr (ANF a) (ANF a)
  | And (ANF a) (ANF a)
  | Var a
  | Lit Bool
  deriving (Show, Eq, Ord, Generic, NFData)

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
prettyBase (Lit True                   ) = text "1"
prettyBase (Lit False                  ) = text "0"

parseBaseANF :: DIMACS Int -> ANF Int
parseBaseANF = toXOr . clauses

-- | @Clause@ = @[Int]@, so this function simply build a tree of left associated
-- variables.
--
-- >>> toAnd [1,2,3] == And (And (Var 1) (Var 2)) (Var 3)
toAnd :: Clause -> ANF Int
toAnd []  = Lit True -- Is this valid?
toAnd cls = foldl1 And . fmap Var $ cls

toXOr :: [Clause] -> ANF Int
toXOr []  = Lit True -- This is valid :D
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
  termToANF (TermQualIdentifier (QIdentifier (ISymbol "true" ))) = Lit True
  termToANF (TermQualIdentifier (QIdentifier (ISymbol "false"))) = Lit False
  termToANF (TermQualIdentifier (QIdentifier (ISymbol sym    ))) = Var sym
  termToANF (TermQualIdentifierT (QIdentifier (ISymbol "xor")) terms) =
    foldl1 XOr (map termToANF terms)
  termToANF (TermQualIdentifierT (QIdentifier (ISymbol "and")) terms) =
    foldl1 And (map termToANF terms)
  termToANF _ = Lit False

---------------

minimizeBase :: Eq a => ANF a -> ANF a
minimizeBase (And left (XOr xleft xright)) =
  XOr (minimizeBase (And left xleft)) (minimizeBase (And left xright))
minimizeBase (And (XOr xleft xright) right) =
  XOr (minimizeBase (And xleft right)) (minimizeBase (And xright right))
minimizeBase (XOr left right) = XOr (minimizeBase left) (minimizeBase right)
-- minimizeBase (And left right) | left == right = left
minimizeBase anf              = anf

---------------
