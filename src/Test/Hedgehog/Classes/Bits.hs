{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.Hedgehog.Classes.Bits
  ( bitsLaws
  ) where

import Data.Bits
import Hedgehog
import Hedgehog.Gen (int)
import Hedgehog.Range (constant)

import Test.Hedgehog.Properties

bitsLaws
  :: forall a. (Eq a, FiniteBits a, Show a)
  => Gen a
  -> PropertyT IO ()
bitsLaws gen = do
  conjunctionIdempotence gen
  disjunctionIdempotence gen
  doubleComplement gen
  setBitLaw gen
  clearBitLaw gen
  complementBitLaw gen
  clearZeroLaw gen
  setZeroLaw gen
  testZeroLaw gen
  popZeroLaw gen
  countLeadingZerosLaw gen
  countTrailingZerosLaw gen

mkBitRange
  :: forall m a. (Monad m, FiniteBits a, Show a)
  => Gen a
  -> PropertyT m (a, Int)
mkBitRange gen = do
  n <- forAll gen
  i <- forAll $ int (constant 0 (finiteBitSize n - 1))
  return (n, i)

conjunctionIdempotence
  :: forall a. (Eq a, FiniteBits a, Show a)
  => Gen a
  -> PropertyT IO ()
conjunctionIdempotence gen = idempotentBinary (.&.) gen

disjunctionIdempotence
  :: forall a. (Eq a, FiniteBits a, Show a)
  => Gen a
  -> PropertyT IO ()
disjunctionIdempotence gen = idempotentBinary (.|.) gen

doubleComplement
  :: forall a. (Eq a, FiniteBits a, Show a)
  => Gen a
  -> PropertyT IO ()
doubleComplement gen = idempotentUnary complement gen

setBitLaw
  :: forall a. (Eq a, FiniteBits a, Show a)
  => Gen a
  -> PropertyT IO ()
setBitLaw gen = do
  (n,i) <- mkBitRange gen 
  n `setBit` i === n .|. bit i

clearBitLaw
  :: forall a. (Eq a, FiniteBits a, Show a)
  => Gen a
  -> PropertyT IO ()
clearBitLaw gen = do
  (n,i) <- mkBitRange gen 
  n `clearBit` i === n .&. complement (bit i)

complementBitLaw
  :: forall a. (Eq a, FiniteBits a, Show a)
  => Gen a
  -> PropertyT IO ()
complementBitLaw gen = do
  (n,i) <- mkBitRange gen 
  complementBit n i === xor n (bit i)

clearZeroLaw
  :: forall a. (Eq a, FiniteBits a, Show a)
  => Gen a
  -> PropertyT IO ()
clearZeroLaw gen = do
  n <- forAll gen
  clearBit n zeroBits === zeroBits

setZeroLaw
  :: forall a. (Eq a, FiniteBits a, Show a)
  => Gen a
  -> PropertyT IO ()
setZeroLaw gen = do
  (_,i) <- mkBitRange gen
  setBit (zeroBits :: a) i === bit i

testZeroLaw
  :: forall a. (Eq a, FiniteBits a, Show a)
  => Gen a
  -> PropertyT IO ()
testZeroLaw gen = do
  (_,i) <- mkBitRange gen
  testBit (zeroBits :: a) i === False

popZeroLaw
  :: forall a. (Eq a, FiniteBits a, Show a)
  => Gen a
  -> PropertyT IO ()
popZeroLaw _ = popCount (zeroBits :: a) === 0

countLeadingZerosLaw
  :: forall a. (Eq a, FiniteBits a, Show a)
  => Gen a
  -> PropertyT IO ()
countLeadingZerosLaw _ = countLeadingZeros (zeroBits :: a) === finiteBitSize (undefined :: a)

countTrailingZerosLaw
  :: forall a. (Eq a, FiniteBits a, Show a)
  => Gen a
  -> PropertyT IO ()
countTrailingZerosLaw _ = countTrailingZeros (zeroBits :: a) === finiteBitSize (undefined :: a)
