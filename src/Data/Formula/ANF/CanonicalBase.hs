{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Formula.ANF.CanonicalBase where

import           Control.DeepSeq                ( NFData )
import           GHC.Generics

import qualified Data.Formula.ANF.Base         as B

newtype ANF a = ANF { unANF :: XOr a}
  deriving (Show, Eq, Ord, Generic, NFData, Functor, Foldable, Traversable)
data XOr a = XOr (And a) (XOr a) | EmptyXOr
  deriving (Show, Eq, Ord, Generic, NFData, Functor, Foldable, Traversable)
data And a = And (Value a) (And a) | EmptyAnd
  deriving (Show, Eq, Ord, Generic, NFData, Functor, Foldable, Traversable)
data Value a = Var a | One | T | F
  deriving (Show, Eq, Ord, Generic, NFData, Functor, Foldable, Traversable)

isCanonical :: B.ANF a -> Bool
isCanonical anf = isCanonicalXOr anf
 where
  isCanonicalXOr :: B.ANF a -> Bool
  isCanonicalXOr (B.XOr left right@(B.XOr _ _)) =
    (isCanonicalAnd left || isCanonicalValue left) && isCanonicalXOr right
  isCanonicalXOr (B.XOr left right) =
    (isCanonicalAnd left || isCanonicalValue left)
      && (isCanonicalAnd right || isCanonicalValue right)
  isCanonicalXOr anf = isCanonicalAnd anf || isCanonicalValue anf

  isCanonicalAnd :: B.ANF a -> Bool
  isCanonicalAnd (B.And left right@(B.And _ _)) = isCanonicalValue left && isCanonicalAnd right
  isCanonicalAnd (B.And left right            ) = isCanonicalValue left && isCanonicalValue right
  isCanonicalAnd _                              = False

  isCanonicalValue :: B.ANF a -> Bool
  isCanonicalValue (B.Var _) = True
  isCanonicalValue (B.T    ) = True
  isCanonicalValue (B.F    ) = True
  isCanonicalValue _         = False

fromBase :: B.ANF a -> Maybe (ANF a)
fromBase anf | not (isCanonical anf) = Nothing
             | otherwise             = Just (fromBase' anf)

fromBase' :: B.ANF a -> ANF a
fromBase' anf = ANF . fbXOr $ anf
 where
  fbXOr :: B.ANF a -> XOr a
  fbXOr (    B.XOr left right) = XOr (fbAnd left) (fbXOr right)
  fbXOr anf@(B.And _    _    ) = XOr (fbAnd anf) EmptyXOr
  fbXOr anf                    = XOr (And (fbValue anf) EmptyAnd) EmptyXOr

  fbAnd :: B.ANF a -> And a
  fbAnd (B.And B.T r  ) = And (fbValue r) EmptyAnd
  fbAnd (B.And l   B.T) = And (fbValue l) EmptyAnd
  fbAnd (B.And l   r  ) = And (fbValue l) (fbAnd r)
  fbAnd anf             = And (fbValue anf) EmptyAnd

  fbValue :: B.ANF a -> Value a
  fbValue (B.Var v) = Var v
  fbValue B.One     = One
  fbValue B.T       = T
  fbValue B.F       = F
