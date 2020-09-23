module Data.Formula.ANF.BaseSpec where

import           Data.Formula.ANF.Base          ( ANF(..)
                                                , baseSat
                                                , smtToBaseANF
                                                )
import           Data.SAT.Smt
import           Data.SAT
import           Smtlib.Syntax.Syntax
import           Test.Hspec

import           Data.List.NonEmpty

spec :: Spec
spec = do
  itSolveSatisfy
  itMinimizesAnANF
  itConvertsSmtToBase

satAnf :: ANF Int
satAnf = XOr (And (Var 1) (Var 3)) (XOr (And (Var 2) (Var 3)) (Var 3))

unsatAnf :: ANF Int
unsatAnf = XOr satAnf satAnf

itSolveSatisfy :: Spec
itSolveSatisfy = do
  let solve = solveSAT baseSat
  describe "Solve satisfy correctly" $ do
    it "Identifies as SAT" $ solve satAnf `shouldBe` Satisfiable
    it "Identifies as UNSAT" $ solve unsatAnf `shouldBe` Unsatisfiable

itMinimizesAnANF :: Spec
itMinimizesAnANF = do
  let minimize' = minimize baseSat
  describe "Minimization" $ do
    it "Leaves a reduced term as it is" $ minimize' (And (T) (Var 1)) `shouldBe` And (T) (Var 1)
    it "min a(b+c) => (min ab) + (min ac)"
      $          minimize' (And (Var 1) (XOr (Var 2) (Var 3)))
      `shouldBe` (XOr (And (Var 1) (Var 2)) (And (Var 1) (Var 3)))

    it "min (b+c)a => (min ab) + (min ac)"
      $          minimize' (And (XOr (Var 2) (Var 3)) (Var 1))
      `shouldBe` (XOr (And (Var 2) (Var 1)) (And (Var 3) (Var 1)))
    it "min a + b  => min a + min b"
      $          minimize' (XOr (Var 1) (Var 2))
      `shouldBe` (XOr (Var 1) (Var 2))
    it "min aa     => min a" $ minimize' (And (Var 1) (Var 1)) `shouldBe` (Var 1)

itConvertsSmtToBase :: Spec
itConvertsSmtToBase = do
  let mkIdentifier = Assert . TermQualIdentifier . QIdentifier
  let mkSymbol     = mkIdentifier . ISymbol
  describe "Convert SMT to BaseANF" $ do
    it "Parses an identifier" $ smtToBaseANF [mkSymbol "a"] `shouldBe` [Var "a"]

    it "Parses a list of identifiers"
      $ let vars = ["b", "c", "d"] in smtToBaseANF (fmap mkSymbol vars) `shouldBe` (fmap Var vars)

    it "Parses a formula (xor with vars)"
      $ let (Right smt) = parseSmt "(assert (xor a b c))"
        in  smtToBaseANF smt `shouldBe` [XOr (XOr (Var "a") (Var "b")) (Var "c")]

    it "Parses a formula (xor with an xor inside)"
      $ let (Right smt) = parseSmt "(assert (xor a (xor b c)))"
        in  smtToBaseANF smt `shouldBe` [XOr (Var "a") (XOr (Var "b") (Var "c"))]

    it "Parses a formula with vars and lits"
      $ let (Right smt) = parseSmt "(assert (xor (and a b c) (and a c) (and b c) true))"
            anf =
              foldl1 XOr
                $  (foldl1 And $ (Var "a") :| [(Var "b"), (Var "c")])
                :| [ foldl1 And $ (Var "a") :| [(Var "c")]
                   , foldl1 And $ (Var "b") :| [(Var "c")]
                   , (T)
                   ]
        in  smtToBaseANF smt `shouldBe` [anf]
