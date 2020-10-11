module Interactive where

import           Data.SAT.Smt                   ( parseSmtFile )
import           Data.Formula.ANF.Base

main :: IO [ANF String]
main = do
  Right smt <- parseSmtFile "./test/files/test0.smt"
  let anf = smtToBaseANF smt
  return anf
