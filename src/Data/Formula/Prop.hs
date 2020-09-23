{-# LANGUAGE DeriveTraversable #-}
module Data.Formula.Prop where

import           Data.Either                    ( rights )

import           Data.SAT.DIMACS                ( DIMACS(..)
                                                , Clause
                                                )

import           Smtlib.Syntax.Syntax           ( Source
                                                , Command(..)
                                                , Term(..)
                                                , QualIdentifier(..) -- QIdentifier
                                                , Identifier(..) -- Symbol
                                                )
import           Test.QuickCheck                ( Arbitrary(..)
                                                , frequency
                                                )

import           Text.PrettyPrint.Leijen        ( (<+>)
                                                , Doc
                                                , Pretty
                                                , pretty
                                                , text
                                                )

data Prop a
  = And (Prop a) (Prop a)
  | Or  (Prop a) (Prop a)
  | Imp (Prop a) (Prop a)
  | Iff (Prop a) (Prop a)
  | XOr (Prop a) (Prop a)
  | Not (Prop a)
  | Var a
  | T
  | F
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Arbitrary a => Arbitrary (Prop a) where
  arbitrary = frequency
    [ (1, And <$> arbitrary <*> arbitrary)
    , (1, Or <$> arbitrary <*> arbitrary)
    , (1, Imp <$> arbitrary <*> arbitrary)
    , (1, Iff <$> arbitrary <*> arbitrary)
    , (1, XOr <$> arbitrary <*> arbitrary)
    , (2, Not <$> arbitrary)
    , (3, Var <$> arbitrary)
    , (3, pure T)
    , (3, pure F)
    ]

instance Pretty a => Pretty (Prop a) where
  pretty = prettyProp

prettyProp :: Pretty a => Prop a -> Doc
prettyProp (And l@(XOr _ _) r@(XOr _ _)) =
  text "(" <> pretty l <> text ")(" <> pretty r <> text ")"
prettyProp (And l@(XOr _ _) r          ) = text "(" <> pretty l <> text ")" <> pretty r
prettyProp (And l           r@(XOr _ _)) = pretty l <> text "(" <> pretty r <> text ")"
prettyProp (And l           r          ) = pretty l <> pretty r
prettyProp (XOr l           r          ) = pretty l <+> (text "⊕" <+> pretty r)
prettyProp (Or  l           r          ) = pretty l <+> (text "∨" <+> pretty r)
prettyProp (Imp l           r          ) = pretty l <+> (text "⇒" <+> pretty r)
prettyProp (Iff l           r          ) = pretty l <+> (text "⇔" <+> pretty r)
prettyProp (Not e                      ) = text "¬" <+> pretty e
prettyProp (Var a                      ) = pretty a
prettyProp T                             = text "T"
prettyProp F                             = text "F"


fromSmt :: Source -> [Prop String]
fromSmt = rights . map commandToProp
 where
  commandToProp :: Command -> Either String (Prop String)
  commandToProp (Assert term) = Right $ termToProp term
  commandToProp _             = Left "Command was not an Assert"

  termToProp :: Term -> Prop String
  termToProp (TermQualIdentifier (QIdentifier (ISymbol "true" ))) = T
  termToProp (TermQualIdentifier (QIdentifier (ISymbol "false"))) = F
  termToProp (TermQualIdentifier (QIdentifier (ISymbol sym    ))) = Var sym
  termToProp (TermQualIdentifierT (QIdentifier (ISymbol "not")) terms) =
    foldl1 And (map (Not . termToProp) terms)
  termToProp (TermQualIdentifierT (QIdentifier (ISymbol "or")) terms) =
    foldl1 Or (map termToProp terms)
  termToProp (TermQualIdentifierT (QIdentifier (ISymbol "and")) terms) =
    foldl1 And (map termToProp terms)
  termToProp (TermQualIdentifierT (QIdentifier (ISymbol "=>")) terms) =
    foldl1 Imp (map termToProp terms)
  termToProp (TermQualIdentifierT (QIdentifier (ISymbol "=")) terms) =
    foldl1 Iff (map termToProp terms)

  termToProp _ = F

-- Remember that a clase is a list of list of integers ([[Int]]). Nested folds :D
fromDimacs :: DIMACS Int -> Prop Int
fromDimacs = toOr . clauses
 where
  toOr :: [Clause] -> Prop Int
  toOr []  = F
  toOr [x] = toAnd x
  toOr xs  = foldl1 Or $ map toAnd xs

  toAnd :: Clause -> Prop Int
  toAnd []  = T
  toAnd [x] = Var x
  toAnd xs  = foldl1 And $ fmap Var xs
