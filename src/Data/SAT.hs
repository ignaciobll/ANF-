module Data.SAT where

import           Data.SAT.DIMACS (DIMACS)

data IsSAT = Satisfiable | Unsatisfiable deriving (Show, Eq)

-- SAT data type
--   form: Formula notation (CNF, ANF, ...)
--   var: Values of vars (commonly Int)
--   parse: Source type to parse form (commonly String or ByteString)
data SAT form var = SAT
  {
    solveSAT      :: form var -> IsSAT,
    minimize      :: form var -> form var,
    solveSolution :: form var -> [var],
    parseFormula  :: DIMACS var -> form var
  }
