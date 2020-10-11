module Utils.Binary where

import qualified Data.List                     as L
import           Data.Char                      ( intToDigit )

dec2Bin :: Integral a => a -> String
dec2Bin = reverse . L.map (intToDigit . fromIntegral) . L.unfoldr
  (\x -> if x == 0 then Nothing else Just (rem x 2, div x 2))


dec2Vars :: Integral a => a -> [a]
dec2Vars p = (\x -> if length x == 1 then [head x, head x] else x) . L.filter (/= 0) $ L.unfoldr
  (\(x, n) -> if x == 0 then Nothing else Just ((2 ^ n) * rem x 2, (div x 2, n + 1)))
  (p, 0)
