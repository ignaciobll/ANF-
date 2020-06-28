{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Formula.ANF.IntRepr where

{- |

  We will reuse the code in IntegerRepr but with a sized integer. This
  means that for this case, our maximun amount of variables is 64.

  Future implementations may work with bit-vectors.

-}

import           Data.Bits
import           Data.Formula.ANF
import           Data.SAT
import           Data.SAT.DIMACS

-- We have also change it to an IntMap
import qualified Data.IntMap.Strict as M

newtype ANF a = ANF { getRepr :: [Repr a] }

intReprSat :: SAT ANF Int
intReprSat = SAT {
    solveSAT = solveIntRepr,
    solveSolution = const [],
    parseFormula = parseIntRepr
  }


{- |

  Implementation does not change, but we can see if perfomance improves
  when we have less than 64 variables.

-}

newtype Repr a = IR { getIR :: Int } deriving (Eq, Ord, Num, Show, Bits)

fromClause :: Clause -> Repr Int
fromClause []     = 0
fromClause (i:is) = 2^i + (fromClause is)

parseIntRepr :: DIMACS (ANF Int) -> ANF Int
parseIntRepr = ANF . (map fromClause . clauses)

type Record = M.IntMap Int

solveIntRepr :: ANF Int -> IsSAT
solveIntRepr anf = if solve' (getRepr anf) > 0 then Satisfiable else Unsatisfiable
  where
    solve' :: [Repr Int] -> Int
    solve' = M.size . (M.filter odd) . (M.fromListWith (+)) . (map (\n -> (getIR n, 1)))
