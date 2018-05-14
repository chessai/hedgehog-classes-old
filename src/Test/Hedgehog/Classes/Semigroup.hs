{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.Hedgehog.Classes.Semigroup
  ( semigroupLaws
  ) where

import Data.Foldable (foldr1)
import Data.Semigroup
import Hedgehog
import Hedgehog.Gen (int, nonEmpty)
import Hedgehog.Range (constantBounded)

import qualified Data.List as List

import Test.Hedgehog.Properties

semigroupLaws
  :: forall a. (Eq a, Semigroup a, Show a)
  => Gen a
  -> PropertyT IO ()
semigroupLaws gen = do
  associativity (<>) gen
  sconcatLaw gen
  stimesLaw gen

sconcatLaw
  :: forall a. (Eq a, Semigroup a, Show a)
  => Gen a
  -> PropertyT IO ()
sconcatLaw gen = do
  let rng = constantBounded :: Range Int
  non <- forAll $ nonEmpty rng gen
  foldr1 (<>) non === sconcat non

stimesLaw
  :: forall a. (Eq a, Semigroup a, Show a)
  => Gen a
  -> PropertyT IO ()
stimesLaw gen = do
  n <- forAll $ int constantBounded 
  a <- forAll gen  
  foldr1 (<>) (List.replicate n a) === stimes n a

