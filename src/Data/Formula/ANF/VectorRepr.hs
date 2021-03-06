module Data.Formula.ANF.VectorRepr where

import qualified Data.Vector                   as V

import           Data.Bits                      ( testBit
                                                , FiniteBits
                                                , Bits
                                                , bit
                                                , setBit
                                                , zeroBits
                                                , finiteBitSize
                                                , (.&.)
                                                )
import           Data.Hashable                  ( hashWithSalt )

import           Data.SAT                       ( IsSAT(..)
                                                , SAT(..)
                                                )
import           Data.SAT.DIMACS

import qualified Data.Formula.ANF.Base         as BANF
import qualified Data.Formula.ANF.CanonicalBase
                                               as C
import           Foreign                        ( Bits(complement) )

{- |

  With this vector implementation we are starting to going down in the
  level of abstractions seeking an efficient way to model a parallel
  solution.

  The key ideas is classifing monomials by a property in a vector
  index. This index points to a secondary array structure with a list
  of monimials. This may be similar to a forest, but may work more
  efficiently in a full parallel environment than a tree.

  Going back to our example, let's imagine this monomials:
    - pq:  110
    - pqr: 111
    - r  : 001
    - qr : 011

  There are 8 possible formulas (or 7 if you ignore formula 0). We are
  goint to store them starting from a 2 length vector.

  0 -> 000, 010, 100, 110
  1 -> 001, 011, 101, 111

-}

newtype VectorRepr a = VR { getVector :: V.Vector [(a, Int)] }
  deriving (Show, Eq)

vectorReprSat :: SAT VectorRepr Int
vectorReprSat = SAT { solveSAT      = solveVectorRepr
                    , minimize      = id
                    , solveSolution = const []
                    , parseFormula  = parseVectorRepr
                    }

fromClause :: Clause -> Int
fromClause = foldr (\i -> (+) (2 ^ i)) 0

hash :: Int -> Int -> Int
hash size key = key * 2654435761 .&. (size - 1)

parseVectorRepr :: DIMACS Int -> VectorRepr Int
parseVectorRepr dimacs =
  let size :: Int -- Power of 2 length
      size = 2 ^ 4

      base :: V.Vector [(Int, Int)] -- [(formula, repetitions)]
      base = V.replicate size []

      clausesInt :: [Int] -- [formula]
      clausesInt = map fromClause (clauses dimacs)

      pos :: Int -> Int -- Position of formula in vector
      pos n = (hashWithSalt 1234 n) .&. (size - 1)
      -- pos n = hash size n
      -- post n = n `mod` size

      modPairs :: [(Int, (Int, Int))] -- [(position, (formula, 1)]
      modPairs = map (\n -> (pos n, (n, 1))) clausesInt

      insertPair :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
      insertPair [] t = [t]
      insertPair ((f, n) : xs) (f', n') | f == f'   = (f, n + n') : xs
                                        | otherwise = (f, n) : (insertPair xs (f', n'))
    -- insertPair = flip (:)
  in  VR $ V.accum insertPair base modPairs

solveVectorRepr :: VectorRepr Int -> IsSAT
solveVectorRepr anf = if solve' anf > 0 then Satisfiable else Unsatisfiable
 where
  solve' :: VectorRepr Int -> Int
  solve' (VR v) = V.length . V.filter (not . any (odd . snd)) $ v

--

-- Canonical
-- fromDimacs :: BANF a => VectorRepr a
-- fromDimacso

fromCanonicalBase :: (Integral a, FiniteBits a) => C.ANF a -> [a]
fromCanonicalBase (C.ANF anf) = fromCanonicalXOr anf

fromCanonicalXOr :: (Integral a, FiniteBits a) => C.XOr a -> [a]
fromCanonicalXOr (C.XOr and right) = fromCanonicalAnd and : fromCanonicalXOr right
fromCanonicalXOr C.EmptyXOr        = []

fromCanonicalAnd :: (Integral a, FiniteBits a) => C.And a -> a -- foldr (\i -> (+) (2 ^ i)) 0
fromCanonicalAnd (C.And (C.Var n) right) = setBit (fromCanonicalAnd right) (fromIntegral n)
fromCanonicalAnd (C.And C.One     right) = zeroBits
fromCanonicalAnd (C.And C.T       right) = zeroBits
fromCanonicalAnd (C.And C.F       right) = undefined -- setBit (fromCanonicalAnd right) 1
fromCanonicalAnd C.EmptyAnd              = zeroBits

toExpandedVars :: FiniteBits a => a -> [Int]
toExpandedVars a = [ n | n <- [0 .. finiteBitSize a], testBit a n ]

toCanonicalAnd :: FiniteBits a => a -> C.And Int
toCanonicalAnd a = foldr (C.And) C.EmptyAnd (fmap C.Var $ toExpandedVars a)

toCanonicalXOr :: FiniteBits a => [a] -> C.XOr Int
toCanonicalXOr = foldr C.XOr C.EmptyXOr . fmap toCanonicalAnd

toCanonicalBase :: FiniteBits a => [a] -> C.ANF Int
toCanonicalBase = C.ANF . toCanonicalXOr

--

-- solveList :: FiniteBits a => [a] -> IsSAT
-- solveList xs = undefined
-- where
