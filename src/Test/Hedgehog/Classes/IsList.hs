{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wall #-}

module Test.Hedgehog.Classes.IsList
  ( isListLaws
  ) where

import GHC.Exts
import Hedgehog
import Hedgehog.Gen (list)
import Hedgehog.Range (linear)

isListLaws
  :: forall a. (Eq a, IsList a, Show a, Show (Item a))
  => Gen a
  -> PropertyT IO ()
isListLaws gen = do
  isListPartialIsomorphism gen
  isListLengthPreservation gen

isListPartialIsomorphism
  :: forall a. (Eq a, IsList a, Show a, Show (Item a))
  => Gen a
  -> PropertyT IO ()
isListPartialIsomorphism gen = do
  a <- forAll $ list (linear 0 100) gen
  fromList (toList a) === a

isListLengthPreservation
  :: forall a. (Eq a, IsList a, Show a, Show (Item a))
  => Gen a
  -> PropertyT IO ()
isListLengthPreservation gen = do
  (xs :: a) <- forAll gen
  let ys = toList xs :: [Item a]
  (fromList ys :: a) === fromListN (length ys) ys
  
  --let ys = toList xs :: [Item a]
  --return () 
  --fromList _ === fromListN (length ys) ys 

  --return () --fromList xs 
  
  --let ys = fromList xs
  --fromList ys === fromListN (length ys) ys

