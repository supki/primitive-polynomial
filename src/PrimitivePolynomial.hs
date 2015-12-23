-- | Primitive polynomial sequences.
--
-- <https://en.wikipedia.org/wiki/Primitive_polynomial_(field_theory)>
-- <http://www.ams.org/journals/mcom/1962-16-079/S0025-5718-1962-0148256-1/S0025-5718-1962-0148256-1.pdf>
module PrimitivePolynomial
  ( sequence
  , Gen(next)
  ) where

import           Data.Bits (FiniteBits(finiteBitSize), Bits(shiftL, shiftR, testBit, xor))
import           Data.Bool (bool)
import           Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.List as List
import           Prelude hiding (sequence)
import           Data.Word (Word8, Word16, Word32, Word64)


-- | An infinite, cycling enumeration of the positive values in the domain.
sequence :: (Gen α, Num α) => [α]
sequence =
  iterate' next 1

iterate' :: (a -> a) -> a -> [a]
iterate' f =
  go where go z = z `seq` z : go (f z)

-- | 'FiniteBits' and 'Num' are approximations of 'finite' and 'field'
-- respectively, as we don't have either in Haskell.
class (FiniteBits α, Num α) => Gen α where
  next :: α -> α

instance Gen Int8 where
  next = gnext [0, 1] 2

instance Gen Int16 where
  next = gnext [0, 1] 2

instance Gen Int32 where
  next = gnext [0, 3] 2

instance Gen Int64 where
  next = gnext [0, 1] 2

instance Gen Word8 where
  next = gnext [0, 2, 3, 4] 1

instance Gen Word16 where
  next = gnext [0, 2, 3, 5] 1

instance Gen Word32 where
  next = gnext [0, 1, 2, 3, 5, 7] 1

instance Gen Word64 where
  next = gnext [0, 1, 3, 4] 1

gnext :: (FiniteBits α, Num α) => [Int] -> Int -> α -> α
gnext taps k n =
  (bool 0 1 (List.foldl' (\acc tap -> acc `xor` testBit n tap) False taps) `shiftL` shiftSize) + n'
 where
  shiftSize =
    finiteBitSize n - k
  n' =
    n `shiftR` 1
