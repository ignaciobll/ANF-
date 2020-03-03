module Data.Formula.ANF.Base where

import           Text.PrettyPrint.Leijen (Doc, Pretty, pretty, text, (<+>))

import           Data.SAT
import           Data.SAT.DIMACS

-- SAT formula in ANF notation with Int variables that can be parsed
-- from a String
type BaseSAT = SAT ANF Integer String

baseSat :: BaseSAT
baseSat = SAT {
    solveSAT = const Unsatisfiable,
    solveSolution = const [],
    parseFormula = parseBaseANF
  }

data ANF a
  = And (ANF a) (ANF a)
  | XOr (ANF a) (ANF a)
  | Var a
  | Lit Bool
  deriving (Show, Eq)

instance Pretty a => Pretty (ANF a) where
  pretty = prettyBase

prettyBase :: Pretty a => ANF a -> Doc
prettyBase (And l@(XOr _ _) r) = (text "(") <> pretty l <> (text ")") <> pretty r
prettyBase (And l r@(XOr _ _)) = pretty l <> (text "(") <> pretty r <> (text ")")
prettyBase (And l r)           = pretty l <> pretty r
prettyBase (XOr l r)           = (pretty l) <+> (text "⊕" <+> pretty r)
prettyBase (Var a)             = pretty a
prettyBase (Lit True)          = text "1"
prettyBase (Lit False)         = text "0"

parseBaseANF :: DIMACS (ANF Integer) -> ANF Integer
parseBaseANF (DIMACS _ _ clauses) = toXOr clauses

toAnd :: Clause -> ANF Integer
toAnd []  = Lit True -- Is this valid?
toAnd cls = (foldl1 And) . (fmap Var) $ cls

toXOr :: [Clause] -> ANF Integer
toXOr []  = Lit True -- This is valid :D
toXOr cls = (foldl1 XOr) . (map toAnd) $ cls
