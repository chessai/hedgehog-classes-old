{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.Hedgehog.Classes.Ord
  ( ordLaws
  ) where

import Hedgehog
import Test.Hedgehog.Properties

ordLaws
  :: forall a. (Ord a, Show a)
  => Gen a
  -> (a -> Gen a)
  -> PropertyT IO ()
ordLaws gena genf = do
  reflexive rel gena
  transitive rel gena genf
  antiSymmetric rel gena genf
  where
    rel = (<=)
