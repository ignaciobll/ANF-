module Main where

import           Text.PrettyPrint.Leijen      (pretty)
import           Utils.Options                (parseArgs, runInput)

import           Data.Formula.ANF.Base        (baseSat)
import           Data.Formula.ANF.IntegerRepr (integerReprSat)
import           Data.Formula.ANF.IntRepr     (intReprSat)
import           Data.Formula.ANF.VectorRepr  (vectorReprSat)

import           Data.SAT                     (SAT (..))
import           Data.SAT.DIMACS

import qualified Criterion
import qualified Criterion.Main               as CriterionMain


main :: IO ()
main = do
--  cmdOpts <- parseArgs
  src <- readFile "benchmarks/test.anf" -- runInput cmdOpts

  case parseAnfDIMACS src of
       Left err     -> putStrLn $ printParseError err
       Right dimacs -> -- print $ (solveSAT intReprSat) $ (parseFormula intReprSat) dimacs
         CriterionMain.defaultMain
         [
           benchDimacs "Base ANF"     baseSat        dimacs
         , benchDimacs "Integer Repr" integerReprSat dimacs
         , benchDimacs "Int Repr"     intReprSat     dimacs
         ,
           benchDimacs "Vector Repr"  vectorReprSat  dimacs
         ]

  where
    benchDimacs :: String -> SAT f v -> DIMACS v -> Criterion.Benchmark
    benchDimacs name sat dimacs =
      let
       solve = solveSAT sat
       parse = parseFormula sat
       anf = parse dimacs
      in
        Criterion.bench name $ Criterion.whnf solve anf
