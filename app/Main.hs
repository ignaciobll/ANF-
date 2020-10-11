module Main where

import           Text.Pretty.Simple             ( pPrint )
import           Text.PrettyPrint.Leijen        ( pretty )
import           Utils.Options                  ( parseArgs
                                                , runInput
                                                )
import           Data.Foldable                  ( toList )

import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import qualified Criterion
import qualified Criterion.Main                as CriterionMain

import           Data.Formula.ANF.Base          ( canonical
                                                , fromProp
                                                , baseSat
                                                )
import           Data.Formula.ANF.IntegerRepr   ( integerReprSat )
import           Data.Formula.ANF.IntRepr       ( intReprSat )
import           Data.Formula.ANF.VectorRepr    ( vectorReprSat )
import           Data.Formula.Prop              ( fromSmt )
import           Data.Formula.ANF.CanonicalBase ( fromBase )
import           Data.Formula.ANF.VectorRepr    ( fromCanonicalBase )

import           Data.SAT                       ( SAT(..) )
import           Data.SAT.DIMACS
import           Data.SAT.Smt                   ( parseSmtFile )

import qualified Data.Formula.ANF.Base         as B

main :: IO ()
main = do
  -- cmdOpts <- parseArgs
  src <- readFile "benchmarks/test.anf" -- runInput cmdOpts

  case parseCnfDIMACS src of
    Left  err    -> putStrLn $ printParseError err
    Right dimacs -> -- print $ (solveSAT intReprSat) $ (parseFormula intReprSat) dimacs
                    CriterionMain.defaultMain
      [ benchDimacs "Base ANF"     baseSat        dimacs
      , benchDimacs "Integer Repr" integerReprSat dimacs
      , benchDimacs "Int Repr"     intReprSat     dimacs
      , benchDimacs "Vector Repr"  vectorReprSat  dimacs
      ]

 where
  benchDimacs :: String -> SAT f v -> DIMACS v -> Criterion.Benchmark
  benchDimacs name sat dimacs =
    let solve = solveSAT sat
        parse = parseFormula sat
        anf   = parse dimacs
    in  Criterion.bench name $ Criterion.whnf solve anf
