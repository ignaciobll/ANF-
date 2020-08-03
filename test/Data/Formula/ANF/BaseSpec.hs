module Data.Formula.ANF.BaseSpec where

import           Test.Hspec

import           Data.Formula.ANF.Base (ANF (..), baseSat)
import           Data.SAT

spec :: Spec
spec = do
  let solve = solveSAT baseSat
  describe "Solve satisfy correctly" $ do
    it "Identifies as SAT" $
      solve satAnf `shouldBe` Satisfiable
    it "Identifies as UNSAT" $
      solve unsatAnf `shouldBe` Unsatisfiable
  itMinimizesAnANF

satAnf :: ANF Int
satAnf = XOr (And (Var 1) (Var 3)) (XOr (And (Var 2) (Var 3)) (Var 3))

unsatAnf :: ANF Int
unsatAnf = XOr satAnf satAnf

itMinimizesAnANF :: Spec
itMinimizesAnANF = do
  let minimize' = minimize baseSat
  describe "Minimization" $ do
    it "Leaves a reduced term as it is" $
      minimize' (And (Lit True) (Var 1)) `shouldBe` And (Lit True) (Var 1)
