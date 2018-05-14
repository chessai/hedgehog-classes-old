{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.Hedgehog.Classes.Monoid
  ( monoidLaws
  , commutativeMonoidLaws 
  ) where

import Data.Monoid
import Data.Semigroup
import Hedgehog
import Hedgehog.Gen (list)
import Hedgehog.Range (constantBounded)

import Test.Hedgehog.Properties

monoidLaws
  :: forall a. (Eq a, Monoid a, Show a)
  => Gen a
  -> PropertyT IO ()
monoidLaws gen = do
  identity mappend mempty gen
  associativity mappend gen
  mconcatLaw gen 
  monoidSemigroupSame gen

mconcatLaw
  :: forall a. (Eq a, Monoid a, Show a)
  => Gen a
  -> PropertyT IO ()
mconcatLaw gen = do
  let rng = constantBounded :: Range Int
  ls <- forAll $ list rng gen
  
  mconcat ls === foldr mappend mempty ls

monoidSemigroupSame
  :: forall a. (Eq a, Monoid a, Show a)
  => Gen a
  -> PropertyT IO ()
monoidSemigroupSame gen = do
  a <- forAll gen
  b <- forAll gen
  mappend a b === a <> b

commutativeMonoidLaws
  :: forall a. (Eq a, Monoid a, Show a)
  => Gen a
  -> PropertyT IO ()
commutativeMonoidLaws gen = do
  monoidLaws gen
  commutativity mappend gen

