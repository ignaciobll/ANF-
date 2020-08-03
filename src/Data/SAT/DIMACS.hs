module Data.SAT.DIMACS (
  DIMACS(..),
  Clause,
  -- cast,
  parseAnfDIMACS,
  parseCnfDIMACS,
  parseDIMACS,
  printParseError,
  genDIMACS,
  pDIMACS,
  pInfoLine
  ) where

import           Data.SAT.DIMACS.Lexer (Parser, integer, lexeme, sc,
                                        signedInteger, symbol)
import           Data.SAT.DIMACS.ANF (pVarANF)
import           Data.SAT.DIMACS.CNF (pVarCNF)


import           Data.Void
import           Text.Megaparsec       (ParseErrorBundle, errorBundlePretty,
                                        many, manyTill, parse, (<|>))
import           Text.Megaparsec.Char  (char)



import           Test.QuickCheck


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
data DIMACS a = DIMACS
  {
    nbvar     :: Int, -- Number of vars
    nbclauses :: Int, -- Number of clauses
    clauses   :: [[a]] -- Clauses
  }
  deriving (Show, Eq)

type Clause = [Int]

-- cast :: DIMACS a -> DIMACS b
-- cast (DIMACS x y z) = DIMACS x y z

parseAnfDIMACS :: String -> Either (ParseErrorBundle String Void) (DIMACS Int)
parseAnfDIMACS = parseDIMACS pVarANF

parseCnfDIMACS :: String -> Either (ParseErrorBundle String Void) (DIMACS Int)
parseCnfDIMACS = parseDIMACS pVarCNF

parseDIMACS :: Parser a -> String -> Either (ParseErrorBundle String Void) (DIMACS a)
parseDIMACS pVar = parse (pDIMACS pVar) ""

printParseError :: ParseErrorBundle String Void -> String
printParseError = errorBundlePretty

pDIMACS :: Parser a -> Parser (DIMACS a)
pDIMACS pVar = do
  sc
  -- Info Line
  (pNbvar, pNbclauses) <- pInfoLine
  -- List of clauses
  pClauses <- many $ pClause pVar
  pure $ DIMACS pNbvar pNbclauses pClauses

pInfoLine :: Parser (Int, Int)
pInfoLine = do
  _ <- symbol "p"
  lexeme (symbol "cnf" <|> symbol "anf")
  pNbvar <- lexeme integer
  pNbclauses <- lexeme integer
  pure (pNbvar, pNbclauses)

pClause :: Parser a -> Parser [a]
pClause pVar = pVar `manyTill` char '0' <* char '\n'

genDIMACS :: Gen a -> Gen (DIMACS a)
genDIMACS genVar = do
  nVars <- (+1) <$> getSize
  nClauses <- (*3) . (+1) <$> getSize
  -- let genVar = (\n -> (mod n nVars) + 1) <$> (arbitrary :: Gen Int)
  let genClause = listOf1 genVar
  clausesList <- vectorOf nClauses genClause
  pure (DIMACS nVars nClauses clausesList)
