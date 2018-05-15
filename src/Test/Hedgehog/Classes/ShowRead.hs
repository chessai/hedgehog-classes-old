{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.Hedgehog.Classes.ShowRead
  ( showReadLaws
  ) where

import Hedgehog
import Text.Read (readMaybe)

showReadLaws
  :: (Eq a, Read a, Show a)
  => Gen a
  -> PropertyT IO ()
showReadLaws gen = do
  x <- forAll gen
  readMaybe (show x) === Just x
