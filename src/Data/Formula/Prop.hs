{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Formula.Prop where

import           GHC.Generics
import           Data.Validity
import           Data.GenValidity

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
import           Test.QuickCheck                ( genericShrink
                                                , Arbitrary(..)
                                                , frequency
                                                )

import           Text.PrettyPrint.Leijen        ( (<+>)
                                                , Doc
                                                , Pretty
                                                , pretty
                                                , text
                                                )
import qualified Data.List                     as L
import           Test.QuickCheck.Gen            ( suchThat )


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
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance Validity a => Validity (Prop a)
instance (Arbitrary a, GenUnchecked a) => GenUnchecked (Prop a) where
  genUnchecked = arbitrary
instance (Arbitrary a, GenValid a, GenUnchecked a) => GenValid (Prop a) where
  genValid = genUnchecked `suchThat` (\prop -> height prop < 10)

instance Arbitrary a => Arbitrary (Prop a) where
  shrink    = shrinkProp
  arbitrary = frequency
    [ (3, And <$> arbitrary <*> arbitrary)
    , (3, Or <$> arbitrary <*> arbitrary)
    , (1, Imp <$> arbitrary <*> arbitrary)
    , (1, Iff <$> arbitrary <*> arbitrary)
    , (1, XOr <$> arbitrary <*> arbitrary)
    , (2, Not <$> arbitrary)
    , (9, Var <$> arbitrary)
    , (5, pure T)
    , (5, pure F)
    ]

height :: Prop a -> Int
height (And l r) = 1 + max (height l) (height r)
height (Or  l r) = 1 + max (height l) (height r)
height (Imp l r) = 1 + max (height l) (height r)
height (Iff l r) = 1 + max (height l) (height r)
height (XOr l r) = 1 + max (height l) (height r)
height (Not p  ) = 1 + height p
height _         = 1

shrinkProp :: Arbitrary a => Prop a -> [Prop a]
shrinkProp T       = [F]
shrinkProp F       = [T]
shrinkProp (Var a) = [T, F] ++ (Var <$> shrink a)
shrinkProp (Not p) = [T, F, p]
shrinkProp (XOr l r) =
  [T, F]
    ++ [ l' | l' <- shrinkProp l ]
    ++ [ r' | r' <- shrinkProp r ]
    ++ [ XOr l' r' | l' <- shrinkProp l, r' <- shrinkProp r ]
shrinkProp (Iff l r) =
  [T, F]
    ++ [ l' | l' <- shrinkProp l ]
    ++ [ r' | r' <- shrinkProp r ]
    ++ [ Iff l' r' | l' <- shrinkProp l, r' <- shrinkProp r ]
shrinkProp (Imp l r) =
  [T, F]
    ++ [ l' | l' <- shrinkProp l ]
    ++ [ r' | r' <- shrinkProp r ]
    ++ [ Imp l' r' | l' <- shrinkProp l, r' <- shrinkProp r ]
shrinkProp (Or l r) =
  [T, F]
    ++ [ l' | l' <- shrinkProp l ]
    ++ [ r' | r' <- shrinkProp r ]
    ++ [ Or l' r' | l' <- shrinkProp l, r' <- shrinkProp r ]
shrinkProp (And l r) =
  [T, F]
    ++ [ l' | l' <- shrinkProp l ]
    ++ [ r' | r' <- shrinkProp r ]
    ++ [ And l' r' | l' <- shrinkProp l, r' <- shrinkProp r ]


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
  termToProp (TermQualIdentifierT (QIdentifier (ISymbol "xor")) terms) =
    foldl1 XOr (map termToProp terms)
  termToProp (TermQualIdentifierT (QIdentifier (ISymbol "and")) terms) =
    foldl1 And (map termToProp terms)
  termToProp (TermQualIdentifierT (QIdentifier (ISymbol "=>")) terms) =
    foldl1 Imp (map termToProp terms)
  termToProp (TermQualIdentifierT (QIdentifier (ISymbol "=")) terms) =
    foldl1 Iff (map termToProp terms)
  termToProp _ = T

-- Remember that a clase is a list of list of integers ([[Int]]). Nested folds :D
fromDimacs :: DIMACS Int -> Prop Int
fromDimacs = toOr . clauses
 where
  toOr :: [Clause] -> Prop Int
  toOr []  = F
  toOr [x] = toAnd x
  toOr xs  = foldr1 Or $ map toAnd xs

  toAnd :: Clause -> Prop Int
  toAnd []  = T
  toAnd [x] = toVar x
  toAnd xs  = foldr1 And $ fmap toVar $ L.sort xs

  toVar :: Int -> Prop Int
  toVar x | x >= 0    = Var x
          | otherwise = Not . Var . abs $ x
----
