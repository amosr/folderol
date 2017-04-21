{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel where

-- Separate the kernels to make viewing Core easier
import Test.Folderol.Kernel.Filter1
import Test.Folderol.Kernel.Map1
import Test.Folderol.Kernel.Map2
import Test.Folderol.Kernel.Map2Ignorant

import P

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Vector as Vector

import System.IO
import           Control.Monad.Trans.Class (MonadTrans(..))


genVec :: Gen IO (Vector.Vector Int)
genVec = Vector.fromList <$> Gen.list (Range.linear 0 100) (Gen.int $ Range.linear 0 100)

prop_increment :: Property
prop_increment = property $ do
  xs <- forAll genVec
  ys <- lift $ map1 (+1) xs
  ys === fmap (+1) xs

prop_increment_2 :: Property
prop_increment_2 = property $ do
  xs <- forAll genVec
  ys <- lift $ map2 (+1) (*2) xs
  ys === fmap (*2) (fmap (+1) xs)

prop_increment_ignorant :: Property
prop_increment_ignorant = property $ do
  xs <- forAll genVec
  ys <- lift $ map2_ignorant (+1) (*2) xs
  ys === fmap (+1) xs

prop_filter :: Property
prop_filter = property $ do
  xs <- forAll genVec
  ys <- lift $ filter1 (>10) xs
  ys === Vector.filter (>10) xs


tests :: IO Bool
tests = $$(checkSequential)
