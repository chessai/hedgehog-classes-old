{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.Hedgehog.Classes.Eq
  ( eqLaws
  ) where

import Hedgehog
import Test.Hedgehog.Properties

eqLaws
  :: forall a. (Eq a, Show a)
  => Gen a
  -> (a -> Gen a)
  -> PropertyT IO ()
eqLaws gena genf = do
  reflexive rel gena
  transitive rel gena genf
  antiSymmetric rel gena genf
  where
    rel = (==)
