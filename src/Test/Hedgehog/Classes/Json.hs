{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.Hedgehog.Classes.Json
  ( jsonLaws
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as AE
import Hedgehog

jsonLaws
  :: forall a. (Eq a, FromJSON a, Show a, ToJSON a)
  => Gen a
  -> PropertyT IO ()
jsonLaws gen = do
  jsonEncodingPartialIsomorphism gen 
  jsonEncodingEqualsValue gen

jsonEncodingPartialIsomorphism
  :: forall a. (Eq a, FromJSON a, Show a, ToJSON a)
  => Gen a
  -> PropertyT IO ()
jsonEncodingPartialIsomorphism gen = do
  a <- forAll gen 
  AE.decode (AE.encode a) === Just a

jsonEncodingEqualsValue
  :: forall a. (Eq a, FromJSON a, Show a, ToJSON a)
  => Gen a
  -> PropertyT IO ()
jsonEncodingEqualsValue gen = do
  a <- forAll gen
  case AE.decode (AE.encode a) of
    Nothing -> True === False
    Just (v :: AE.Value) -> v === toJSON a
