{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel where

-- Separate the kernels to make viewing Core easier
import Test.Folderol.Kernel.Cycle
import Test.Folderol.Kernel.Filter1
import Test.Folderol.Kernel.FilterMap
import Test.Folderol.Kernel.Map1
import Test.Folderol.Kernel.Map2
import Test.Folderol.Kernel.Map2Ignorant
import Test.Folderol.Kernel.PartitionAppend
import Test.Folderol.Kernel.Zip1
import Test.Folderol.Kernel.ZipSelf
import Test.Folderol.Kernel.ZipWith3

import P

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Vector as Vector

import System.IO
import           Control.Monad.Trans.Class (MonadTrans(..))


genVec :: Gen IO (Vector.Vector Int)
genVec = Vector.fromList <$> Gen.list (Range.linear 0 100) (Gen.int $ Range.linear 0 100)

prop_cycle3 :: Property
prop_cycle3 = property $ do
  xs <- forAll genVec
  ys <- lift $ cycle3 xs
  ys === (Vector.zip (xs <> xs) xs)

prop_map1 :: Property
prop_map1 = property $ do
  xs <- forAll genVec
  ys <- lift $ map1 (+1) xs
  ys === fmap (+1) xs

prop_map2 :: Property
prop_map2 = property $ do
  xs <- forAll genVec
  ys <- lift $ map2 (+1) (*2) xs
  ys === fmap (*2) (fmap (+1) xs)

prop_map2_ignorant :: Property
prop_map2_ignorant = property $ do
  xs <- forAll genVec
  ys <- lift $ map2_ignorant (+1) (*2) xs
  ys === fmap (+1) xs

prop_filter1 :: Property
prop_filter1 = property $ do
  xs <- forAll genVec
  ys <- lift $ filter1 (>10) xs
  ys === Vector.filter (>10) xs

prop_filterMap :: Property
prop_filterMap = property $ do
  xs <- forAll genVec
  (trues,others) <- lift $ filterMap (>10) (*2) xs
  trues  === Vector.filter (>10) xs
  others === fmap (*2) xs


prop_partitionAppend :: Property
prop_partitionAppend = property $ do
  xs <- forAll genVec
  ys <- lift $ partitionAppend f xs
  ys === go xs
 where
  f :: Int -> Bool
  f = (>10)

  go as
   = let bs = Vector.filter f as
         cs = Vector.filter (not . f) as
         ds = bs <> cs
     in  ds


prop_zip1 :: Property
prop_zip1 = property $ do
  xs <- forAll genVec
  ys <- fmap show <$> forAll genVec
  zs <- lift $ zip1 xs ys
  zs === Vector.zip xs ys

prop_zipSelf :: Property
prop_zipSelf = property $ do
  xs <- forAll genVec
  ys <- lift $ zipSelf xs
  ys === Vector.zip xs xs


prop_zipWith3 :: Property
prop_zipWith3 = property $ do
  xs <- forAll genVec
  ys <- lift $ zipWith3 f g h xs
  ys === zw3 xs
 where
  f :: Int -> Double
  f = fromIntegral
  g :: Int -> Int
  g = (+1)
  h :: Double -> Int -> Double
  h b c = b - fromIntegral c

  zw3 as
   = let bs = fmap f as
         cs = fmap g as
         ds = Vector.zipWith h bs cs
     in  ds

tests :: IO Bool
tests = $$(checkConcurrent)
