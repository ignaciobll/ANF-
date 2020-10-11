module Data.Formula.ANF.BaseSpec where

import           Debug.Trace

import           Data.List.NonEmpty
import           Smtlib.Syntax.Syntax
import           Test.Hspec
import           Test.QuickCheck

import           Data.Formula.ANF.Base          ( fromProp
                                                , ANF(..)
                                                , baseSat
                                                , smtToBaseANF
                                                , isCanonical
                                                , fixpoint
                                                )
import           Data.SAT.DIMACS                ( DIMACS(..) )
import           Data.SAT.Smt
import           Data.SAT
import           Data.Formula.Prop              ( Prop
                                                , fromDimacs
                                                )
import           Data.GenValidity               ( GenValid(genValid) )
import           Data.Formula.Prop              ( height )




spec :: Spec
spec = do
  itSolveSatisfy
  itMinimizesAnANF
  itConvertsSmtToBase
  itMinimizeToCanonical

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
    it "Leaves a reduced term as it is" $ minimize' (And (Var 1) T) `shouldBe` And (Var 1) T
    it "min a(b+c) => (min ab) + (min ac)"
      $          minimize' (And (Var 1) (XOr (Var 2) (Var 3)))
      `shouldBe` (XOr (And (Var 1) (Var 2)) (And (Var 1) (Var 3)))

    it "min (b+c)a => (min ab) + (min ac)"
      $          minimize' (And (XOr (Var 2) (Var 3)) (Var 1))
      `shouldBe` (XOr (And (Var 1) (Var 2)) (And (Var 1) (Var 3)))
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

itMinimizeToCanonical :: Spec
itMinimizeToCanonical = do
  let dimacs = DIMACS { nbvar = 2, nbclauses = 3, clauses = [[1, 2, 3], [2, 3], [1, 3]] }
  let anf    = fromProp . fromDimacs $ dimacs
  let m      = minimize baseSat
  it "Should reduce properly a term" $ (m . m . m . m $ anf) `shouldBe` (m . m . m . m . m $ anf)

  it "Reduces to canonical any formula"
    $ property
    $ verboseCheck
    $ forAll
        (          (arbitrary :: Gen (Prop Int))
        `suchThat` (\prop -> height (trace ("Height for: " ++ show prop) prop) <= 5)
        )
    $ \prop -> isCanonical . minimize baseSat . fromProp $ trace ("Quichecking: " ++ show prop) prop
