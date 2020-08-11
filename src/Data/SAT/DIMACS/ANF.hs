module Data.SAT.DIMACS.ANF (
  ANFV,
  pVarANF
  ) where

import           Data.SAT.DIMACS.Lexer (Parser, signedInteger, symbol)
import           Text.Megaparsec ((<|>))

data ANFV = Var Int | Lit Bool deriving (Show, Eq)

pVarANF :: Parser ANFV
pVarANF = (Var <$> signedInteger) <|> (symbol "T" >> pure (Lit True))
