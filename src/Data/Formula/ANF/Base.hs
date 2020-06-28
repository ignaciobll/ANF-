{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Formula.ANF.Base where

import           Data.SAT                (IsSAT (..), SAT (..))
import           Data.SAT.DIMACS

import qualified Data.Map.Strict         as M

import           Criterion               (Benchmark)
import qualified Criterion
import           Text.PrettyPrint.Leijen (Doc, Pretty, pretty, text, (<+>))

import           Control.DeepSeq         (NFData)
import           GHC.Generics

-- SAT formula in ANF notation with Int variables that can be parsed
-- from a String
type BaseSAT = SAT ANF Int

baseSat :: BaseSAT
baseSat = SAT {
    solveSAT = solve,
    solveSolution = const [],
    parseFormula = parseBaseANF
  }

data ANF a
  = XOr (ANF a) (ANF a)
  | And (ANF a) (ANF a)
  | Var a
  | Lit Bool
  deriving (Show, Eq, Ord, Generic, NFData)

instance Pretty a => Pretty (ANF a) where
  pretty = prettyBase

prettyBase :: Pretty a => ANF a -> Doc
prettyBase (And l@(XOr _ _) r) = (text "(") <> pretty l <> (text ")") <> pretty r
prettyBase (And l r@(XOr _ _)) = pretty l <> (text "(") <> pretty r <> (text ")")
prettyBase (And l r)           = pretty l <> pretty r
prettyBase (XOr l r)           = (pretty l) <+> (text "⊕" <+> pretty r)
prettyBase (Var a)             = pretty a
prettyBase (Lit True)          = text "1"
prettyBase (Lit False)         = text "0"

parseBaseANF :: DIMACS (ANF Int) -> ANF Int
parseBaseANF (DIMACS _ _ clauses) = toXOr clauses

toAnd :: Clause -> ANF Int
toAnd []  = Lit True -- Is this valid?
toAnd cls = (foldl1 And) . (fmap Var) $ cls

toXOr :: [Clause] -> ANF Int
toXOr []  = Lit True -- This is valid :D
toXOr cls = (foldl1 XOr) . (map toAnd) $ cls

---------------- Solver ------------------

{- |

  We want to check if there is a combination of variable that satisfy
  this formula. This function will return either SAT or UNSAT.

  The method to achieve the answer will be to keep track of the number
  of monomial occurrences in an ANF formula. If it doesn't collapse
  when simplifying repeated formulas, the answer is SAT.

  We are going to store this formulas as a set of pairs
   - (formula, number of ocurrences)
  Working in modulo 2 arithmetic to count them.

-}

xor :: Bool -> Bool -> Bool
True `xor` True   = False
True `xor` False  = True
False `xor` True  = True
False `xor` False = False

infixr 4 `xor`

type Record = M.Map (ANF Int) Int

solve :: ANF Int -> IsSAT
solve anf = if solve' anf > 0 then Satisfiable else Unsatisfiable
  where
    solve' :: ANF Int -> Int
    solve' f = M.size . (M.filter odd) $ build M.empty f

    build :: Record -> ANF Int -> Record
    build m (XOr l r) = m `merge` (build M.empty l) `merge` (build M.empty r)
    build m other     = M.insertWith (+) other 1 m

    merge :: Record -> Record -> Record
    merge l r = M.unionWith (+) l r

{-

  Now let's try to benchmark this execution. Maybe we should end with
  a data structure that also includes a function to make a benchmark
  from the data structure. Until I learn more about Criterion, I will
  simply add a @setupEnv@ to help.

-}

baseEnv :: IO (ANF Int) -> Benchmark
baseEnv anfIO = Criterion.env anfIO fromBase2Benchmark
  where
    fromBase2Benchmark :: ANF Int -> Benchmark
    fromBase2Benchmark anf =
      Criterion.bench "Base ANF (whnf)" $ Criterion.whnf solve anf

runBaseBenchmark :: IO (DIMACS a) -> Benchmark
runBaseBenchmark dimacsIO = do
  let cast' = cast :: DIMACS a -> DIMACS (ANF Int)
  let anfIO = (parseBaseANF . cast') <$> dimacsIO
  baseEnv anfIO
