{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.Hedgehog.Classes.Integral
  ( integralLaws
  ) where

import Hedgehog

integralLaws
  :: forall a. (Eq a, Integral a, Show a)
  => Gen a
  -> PropertyT IO ()
integralLaws gen = do
  quotientRemainder gen
  divisionModulus gen
  integerRoundtrip gen

quotientRemainder
  :: forall a. (Eq a, Integral a, Show a)
  => Gen a
  -> PropertyT IO ()
quotientRemainder gen = do
  x <- forAll gen
  y <- forAll gen
  (quot x y) * y + (rem x y) === x

divisionModulus
  :: forall a. (Eq a, Integral a, Show a)
  => Gen a
  -> PropertyT IO ()
divisionModulus gen = do
  x <- forAll gen
  y <- forAll gen
  (div x y) * y + (mod x y) === x

integerRoundtrip
  :: forall a. (Eq a, Integral a, Show a)
  => Gen a
  -> PropertyT IO ()
integerRoundtrip gen = do
  x <- forAll gen
  fromInteger (toInteger x) === x
