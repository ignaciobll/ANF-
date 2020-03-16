module Main where

import           Text.PrettyPrint.Leijen (pretty)
import           Utils.Options           (parseArgs, runInput)

import           Data.Formula.ANF.Base   (baseSat)
import           Data.SAT                (SAT (..))
import           Data.SAT.DIMACS


main :: IO ()
main = do
  cmdOpts <- parseArgs
  src <- runInput cmdOpts

  let (SAT solve _ parse) = baseSat

  case parseDIMACS src of
       Left err     -> putStrLn $ printParseError err
       Right dimacs -> print $ solve $ parse dimacs
