module Data.Formula.ANF where

import           Text.PrettyPrint.Leijen

data ANF a
  = And (ANF a) (ANF a)
  | XOr (ANF a) (ANF a)
  | Var a
  | Lit Bool
  deriving (Show, Eq)

instance Pretty a => Pretty (ANF a) where
  pretty (And l@(XOr _ _) r) = (text "(") <> pretty l <> (text ")") <> pretty r
  pretty (And l r@(XOr _ _)) = pretty l <> (text "(") <> pretty r <> (text ")")
  pretty (And l r)           = pretty l <> pretty r
  pretty (XOr l r)           = (pretty l) <+> (text "⊕" <+> pretty r)
  pretty (Var a)             = pretty a
  pretty (Lit True)          = text "1"
  pretty (Lit False)         = text "0"
