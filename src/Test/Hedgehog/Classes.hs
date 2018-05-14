{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.Hedgehog.Classes
  ( bitsLaws
  , eqLaws
  , monoidLaws
  , commutativeMonoidLaws
  , ordLaws
  , semigroupLaws
  ) where

import Test.Hedgehog.Classes.Bits
import Test.Hedgehog.Classes.Eq
import Test.Hedgehog.Classes.Monoid
import Test.Hedgehog.Classes.Ord
import Test.Hedgehog.Classes.Semigroup

