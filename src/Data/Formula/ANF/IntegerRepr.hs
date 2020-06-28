{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Formula.ANF.IntegerRepr where

{- |

  In this module we will use some properties of boolean formulas to
  encode a proposition. Each variable will have assigned a natural
  position in a bit sequence. The max length of the binary number will
  be the number of variables (n). Then we can convert this to a
  natural value with max value @2^(n+1)-1@.

  For example, a formula that may contain variables p, q and r; could
  be encoded as:
    - pq:  110
    - pqr: 111
    - r  : 001

  The main advantage with this is that xor operations are porformed
  really fast. Possible downsides are memory usage, as maybe there are
  other methods to represent more efficiently this formulas.
-}

import           Data.Bits
import           Data.Formula.ANF
import           Data.SAT
import           Data.SAT.DIMACS

import qualified Data.Map.Strict  as M

newtype ANF a = ANF { getRepr :: [Repr a] }

integerReprSat :: SAT ANF Int
integerReprSat = SAT {
    solveSAT = solveIntRepr,
    solveSolution = const [],
    parseFormula = parseIntRepr
  }


{- |

  The first thing that we need to do is convert a clause to a number.

  Integers in haskell may be represented in two ways:
    - S# Int# if number fits in a single Int type.
    - J# Int# ByteArray# if not.

  Remember:
  >>> type Clause = [Int]

  There is also a thing to notice: Clauses are natural numbers that
  doesn't contain value 0.

  An example of this transformation could be:
  >>> fromClause [1,5]
  34

  But this may not be a very useful visualization. We could have an
  specific visualization for this representation. Let's call it
  @Repr Int@. For convenience, can derive its @Num@ and @Bits@ instances.
-}



newtype Repr a = IR { getIR :: Integer } deriving (Eq, Ord, Num, Show, Bits)

fromClause :: Clause -> Repr Int
fromClause []     = 0
fromClause (i:is) = 2^i + (fromClause is)

{- |

  We can easily apply this function to build our data structure from
  the DIMACS data.

  Currently we are only considering its cluses, but in more
  improvements of this algorithm we can work with fixed sizes as we
  know them from before.

-}

parseIntRepr :: DIMACS (ANF Int) -> ANF Int
parseIntRepr = ANF . (map fromClause) . clauses

{- |

  Once we have a list of clauses, we need to group them to count the
  number of times one formula is present. We can use the same
  technique as in Base.

-}

type Record = M.Map (Repr Int) Int

solveIntRepr :: ANF Int -> IsSAT
solveIntRepr anf = if solve' (getRepr anf) > 0 then Satisfiable else Unsatisfiable
  where
    solve' :: [Repr Int] -> Int
    solve' = M.size . (M.filter odd) . (M.fromListWith (+)) . (map (\n -> (n, 1)))
