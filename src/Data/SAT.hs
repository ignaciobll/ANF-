module Data.SAT where

import           Data.SAT.DIMACS (DIMACS)

data IsSAT = Satisfiable | Unsatisfiable deriving (Show, Eq)

-- SAT data type
--   form: Formula notation (CNF, ANF, ...)
--   var: Values of vars (commonly Int)
--   parse: Source type to parse form (commonly String or ByteString)
data SAT form var parse = SAT
  {
    solveSAT      :: form var -> IsSAT,
    solveSolution :: form var -> [var],
    parseFormula  :: DIMACS (form var) -> form var
  }
