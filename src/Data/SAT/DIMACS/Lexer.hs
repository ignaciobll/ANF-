module Data.SAT.DIMACS.Lexer where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char       (char, digitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- | Space Consumer
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "c ")
  (L.skipBlockComment "c\n" "c\n")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

signedInteger :: Parser Int
signedInteger = L.signed sc integer
