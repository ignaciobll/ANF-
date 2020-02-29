module Data.SAT.DIMACS where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char       (char, digitChar, space1, string)
import           Text.Megaparsec.Char.Lexer (skipBlockComment, skipLineComment)

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
    nbvar     :: Int,    -- Number of vars
    nbclauses :: Int,    -- Number of clauses
    clauses   :: [Clause] -- Clauses
  }
  deriving (Show, Eq)

type Clause = [Int]

morf :: DIMACS a -> DIMACS b
morf (DIMACS x y z) = DIMACS x y z

type DMParser = Parsec Void String

parseDIMACS :: DMParser (DIMACS a)
parseDIMACS = do
  (nVars, nClauses) <- parseInfoLine
  cls <- parseClause `endBy` char '\n'
  pure $ DIMACS { nbvar = nVars, nbclauses = nClauses, clauses = cls}

parseCommentDIMACS :: DMParser ()
parseCommentDIMACS = many (skipLineComment "c") >> pure ()

parseNum :: DMParser Int
parseNum = read <$> many digitChar

parseInfoLine :: DMParser (Int, Int)
parseInfoLine = do
  char 'p' >> space1 >> (string "cnf" <|> string "anf") >> space1
  nVars <- parseNum
  space1
  nClauses <- parseNum
  char '\n'
  pure (nVars, nClauses)

parseClause :: DMParser [Int]
parseClause = parseNum `sepBy` char ' '
