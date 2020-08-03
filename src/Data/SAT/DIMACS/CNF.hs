module Data.SAT.DIMACS.CNF (
  pVarCNF
  ) where

import           Data.SAT.DIMACS.Lexer (Parser, signedInteger)

pVarCNF :: Parser Int
pVarCNF = signedInteger
