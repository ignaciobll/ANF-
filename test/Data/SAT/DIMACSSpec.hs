module Data.SAT.DIMACSSpec where

import           Test.Hspec

import           Data.SAT.DIMACS

import           Text.Megaparsec (parse, parseMaybe, parseTest)

singleLineComment = "c This is a comment"
multiLineComment = (unlines . take 3 . repeat) singleLineComment

spec :: Spec
spec = do
  describe "Parse DIMACS file" $ do
    -- it "parses a comment line" $ do
      -- parseMaybe parseCommentDIMACS singleLineComment `shouldBe` (Just ())

    -- it "parses multiline comment" $ do
      -- parseTest parseCommentDIMACS multiLineComment
      -- parseMaybe parseCommentDIMACS multiLineComment `shouldNotBe` Nothing

    it "fails if no 'p' is found" $ do -- Comments not yet supported
      let dimac = parse parseDIMACS "" multiLineComment
      dimac `shouldNotBe` (Right undefined)

    it "parses the info line" $ do
      let dimac = parse parseInfoLine "" "p cnf 3 2\n"
      dimac `shouldBe` Right (3, 2)

    it "parses a well formed DIMAC file" $ do
      let dimac = parse parseDIMACS "" "p anf 2 2\n1 2 0\n2 0\n"
      dimac `shouldBe` (Right $ DIMACS {nbvar = 2, nbclauses = 2, clauses = [[1,2,0],[2,0]]})
