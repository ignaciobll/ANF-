module Data.SAT.DIMACS.ANF (
  pVarANF
  ) where

import           Data.SAT.DIMACS.Lexer (Parser, signedInteger)

data ValueANF = Var Int | Lit Bool deriving (Show, Eq)

pVarANF :: Parser Int
pVarANF = signedInteger
