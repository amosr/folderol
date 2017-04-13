{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-matches -Wno-unused-local-binds #-}
module Test.Folderol.Process.Map where

import Folderol hiding (filter)
import qualified Folderol as F
import Folderol.Splice
import P

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import System.IO
import           Control.Monad.Trans.Class (MonadTrans(..))


map1 :: (a -> b) -> [a] -> IO [b]
map1 f =
 $$(fuseList_1_1 $ \as -> do
    map [||f||] as)

map2 :: (a -> b) -> (b -> c) -> [a] -> IO [c]
map2 f g =
 $$(fuseList_1_1 $ \as -> do
  bs <- map [||f||] as
  cs <- map [||g||] bs
  return cs)

map2_ignorant :: (a -> b) -> (b -> c) -> [a] -> IO [b]
map2_ignorant f g =
 $$(fuseList_1_1 $ \as -> do
  bs <- map [||f||] as
  cs <- map [||g||] bs
  return bs)

filter1 :: (a -> Bool) -> [a] -> IO [a]
filter1 f =
 $$(fuseList_1_1 $ F.filter [||f||])


prop_increment :: Property
prop_increment = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int $ Range.linear 0 100)
  ys <- lift $ map1 (+1) xs
  ys === fmap (+1) xs

prop_increment_2 :: Property
prop_increment_2 = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int $ Range.linear 0 100)
  ys <- lift $ map2 (+1) (*2) xs
  ys === fmap (*2) (fmap (+1) xs)

prop_increment_ignorant :: Property
prop_increment_ignorant = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int $ Range.linear 0 100)
  ys <- lift $ map2_ignorant (+1) (*2) xs
  ys === fmap (+1) xs

prop_filter :: Property
prop_filter = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int $ Range.linear 0 100)
  ys <- lift $ filter1 (>10) xs
  ys === filter (>10) xs



tests :: IO Bool
tests = $$(checkSequential)
