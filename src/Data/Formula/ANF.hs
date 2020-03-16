{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Data.Formula.ANF where

import           Data.Mod.Word   (Mod, unMod)
import           Numeric.Natural (Natural)

type F2 = Mod 2

-- | The serial number of an arbitrarty a in F2^n is
--
--   \Sum_{i=1}{n} a_i * 2^{n-i}
serial :: [F2] -> Int
serial a = sum
  . (map (\(i, ai) -> ai * (2^(n-i))))
  . (zipWith (,) [1..]) -- Index
  . (map (fromIntegral . unMod)) $ a
  where
    n = length a
