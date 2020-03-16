module Data.SAT.DIMACSSpec where

import           Test.Hspec

import           Data.SAT.DIMACS
import           Data.SAT.DIMACS.Lexer (Parser)

import           Text.Megaparsec       (Parsec, parse, parseTest)

singleLineComment = "c This is a comment"
multiLineComment = (unlines . take 3 . repeat) singleLineComment

exampleFile = unlines [
  "c This is a comment line",
  "c Another Comment...",
  "p cnf 2 1",
  "1 -2 0",
  ""]

invalidFile = unlines [
  "c This is a comment line",
  "c Another Comment...",
  "p cnf 2 2",
  "1 -2 0 2",
  "1 -2 0",
   ""]

spec :: Spec
spec = do
  describe "Parse DIMACS file" $ do
    it "fails if no 'p' is found" $ do -- Comments not yet supported
      let dimac = parse pDIMACS "" multiLineComment
      dimac `shouldNotBe` (Right undefined)

    it "parses the info line" $ do
      testParse "p anf 2 12\n" pInfoLine (2, 12)

    it "parses the info line" $ do
      let dimac = parse pInfoLine "" "p cnf 3 2\n"
      dimac `shouldBe` Right (3, 2)

    it "parses a well formed DIMAC file (anf)" $ do
      testParse "p anf 2 2\n1 2 0\n2 0\n" pDIMACS (DIMACS {nbvar = 2, nbclauses = 2, clauses = [[1,2],[2]]})

    it "parses a well formed DIMAC file (cnf)" $ do
      testParse exampleFile pDIMACS (DIMACS {nbvar = 2, nbclauses = 1, clauses = [[1, -2]]})

    it "Fails on invalid file (variable after 0)" $ do
      let dimac = parse pDIMACS "" invalidFile
      parseTest pDIMACS invalidFile
      dimac `shouldNotBe` (Right undefined)

testParse :: (Show a, Eq a) => String -> Parser a -> a -> IO ()
testParse src p result = do
  let dimac = parse p "" src
  dimac `shouldBe` (Right result)
