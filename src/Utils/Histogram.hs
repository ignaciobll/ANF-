module Utils.Histogram where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

newtype Histogram a = Histogram { histogram :: Map a Int } deriving (Show, Eq)

mkHistogram :: (Foldable f, Ord a) => f a -> Histogram a
mkHistogram = Histogram . foldr insertValue Map.empty
 where
  insertValue :: Ord a => a -> Map a Int -> Map a Int
  insertValue k = Map.insertWith (+) k 1
