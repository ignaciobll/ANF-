
module Utils.VarsTable where

import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap


newtype VarsTable a = VarsTable { unVarsTable :: (Int, M.Map a Int, IntMap a)} deriving (Show, Eq)

{- Getters -}

getNVars :: VarsTable a -> Int
getNVars (VarsTable (nVars, _, _)) = nVars

getV2Int :: VarsTable a -> M.Map a Int
getV2Int (VarsTable (_, v2i, _)) = v2i

getInt2V :: VarsTable a -> IntMap a
getInt2V (VarsTable (_, _, i2v)) = i2v

getV2IntList :: VarsTable a -> [(a, Int)]
getV2IntList = M.toAscList . getV2Int

getInt2VList :: VarsTable a -> [(Int, a)]
getInt2VList = IntMap.toAscList . getInt2V

{- Constructor -}

emptyVarsTable :: Ord a => VarsTable a
emptyVarsTable = VarsTable (0, M.empty, IntMap.empty)

varsToInt :: (Foldable f, Ord a) => f a -> VarsTable a
varsToInt = varsToIntWith emptyVarsTable

varsToIntWith :: (Foldable f, Ord a) => VarsTable a -> f a -> VarsTable a
varsToIntWith = foldr (flip addVar)

{- Operators -}

addVar :: Ord a => VarsTable a -> a -> VarsTable a
addVar vt@(VarsTable (i, a2i, i2a)) var = case M.lookup var a2i of
  Nothing -> VarsTable ((i + 1), (M.insert var i a2i), (IntMap.insert i var i2a))
  Just _  -> vt

substituteVars :: Ord a => M.Map a Int -> a -> Int
substituteVars m = (M.!) m

replaceVars :: (Functor f, Ord a) => VarsTable a -> f a -> f Int
replaceVars (VarsTable (_, a2i, _)) f = fmap (a2i M.!) f

getOrElse :: Maybe a -> a -> a
getOrElse (Just a) = const a
getOrElse Nothing  = id

restoreVarWithDefault :: Ord a => VarsTable a -> a -> Int -> a
restoreVarWithDefault vt dft i = getOrElse (getInt2V vt IntMap.!? i) dft

restoreVarsWithDefault :: (Functor f, Ord a) => VarsTable a -> a -> f Int -> f a
restoreVarsWithDefault vt dft = fmap (restoreVarWithDefault vt dft)

restoreVarsMaybe :: (Functor f, Ord a) => VarsTable a -> a -> f Int -> f (Maybe a)
restoreVarsMaybe vt dft = fmap (getInt2V vt IntMap.!?)

restoreVars' :: (Functor f, Ord a) => VarsTable a -> f Int -> f a
restoreVars' vt = fmap (getInt2V vt IntMap.!)
