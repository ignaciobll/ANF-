module Data.Formula.ANF.BaseSpec where

import           Test.Hspec

import           Data.Formula.ANF.Base (ANF (..), baseSat)
import           Data.SAT

spec :: Spec
spec = do
  let (SAT solve _ _) = baseSat
  describe "Solve satisfy correctly" $ do
    it "Identifies as SAT" $
      solve satAnf `shouldBe` Satisfiable
    it "Identifies as UNSAT" $
      solve unsatAnf `shouldBe` Unsatisfiable

satAnf :: ANF Int
satAnf = XOr (And (Var 1) (Var 3)) (XOr (And (Var 2) (Var 3)) (Var 3))

unsatAnf :: ANF Int
unsatAnf = XOr satAnf satAnf
