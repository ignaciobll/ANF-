module Data.SAT.DIMACS where

import           Data.SAT.DIMACS.Lexer (Parser, integer, lexeme, sc,
                                        signedInteger, symbol)

import           Data.Void
import           Text.Megaparsec       (ParseErrorBundle, errorBundlePretty,
                                        many, manyTill, parse, (<|>))
import           Text.Megaparsec.Char  (char)


-- | Example of DIMACS format
--
-- @
--   c
--   c start with comments
--   c
--   c
--   p cnf 5 3
--   1 -5 4 0
--   -1 5 3 4 0
--   -3 -4 0
-- @
--
-- 'a' is a Phantom Type (I think) that only is used to typecheck
-- different formats It only has semantical meaning, so any
-- transformation may be applied with an identity function
data DIMACS a = DIMACS
  {
    nbvar     :: Int, -- Number of vars
    nbclauses :: Int, -- Number of clauses
    clauses   :: [Clause] -- Clauses
  }
  deriving (Show, Eq)

type Clause = [Int]

cast :: DIMACS a -> DIMACS b
cast (DIMACS x y z) = DIMACS x y z

parseDIMACS :: String -> Either (ParseErrorBundle String Void) (DIMACS a)
parseDIMACS src = parse pDIMACS "" src

printParseError :: ParseErrorBundle String Void -> String
printParseError bundle = errorBundlePretty bundle

pDIMACS :: Parser (DIMACS a)
pDIMACS = do
  sc
  -- Info Line
  (nbvar, nbclauses) <- pInfoLine
  -- List of clauses
  clauses <- many pClause
  pure $ DIMACS nbvar nbclauses clauses

pInfoLine :: Parser (Int, Int)
pInfoLine = do
  symbol "p"
  lexeme (symbol "cnf" <|> symbol "anf")
  nbvar <- lexeme integer
  nbclauses <- lexeme integer
  pure (nbvar, nbclauses)

pClause :: Parser [Int]
pClause = signedInteger `manyTill` char '0' <* char '\n'
