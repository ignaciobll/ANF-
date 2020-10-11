
module Utils.VarsTable where

import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap


newtype VarsTable a = VarsTable { unVarsTable :: (Int, M.Map a Int, IntMap a)} deriving (Show, Eq)

addVar :: Ord a => VarsTable a -> a -> VarsTable a
addVar vt@(VarsTable (i, a2i, i2a)) var = case M.lookup var a2i of
  Nothing -> VarsTable ((i + 1), (M.insert var i a2i), (IntMap.insert i var i2a))
  Just _  -> vt

emptyVarsTable :: Ord a => VarsTable a
emptyVarsTable = VarsTable (0, M.empty, IntMap.empty)

varsToInt :: (Foldable f, Ord a) => f a -> VarsTable a
varsToInt = varsToIntWith emptyVarsTable

varsToIntWith :: (Foldable f, Ord a) => VarsTable a -> f a -> VarsTable a
varsToIntWith = foldr (flip addVar)

substituteVars :: Ord a => M.Map a Int -> a -> Int
substituteVars m = (M.!) m

replaceVars :: (Functor f, Ord a) => VarsTable a -> f a -> f Int
replaceVars (VarsTable (_, a2i, _)) f = fmap (a2i M.!) f
