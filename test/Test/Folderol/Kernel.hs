{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Kernel where

-- Separate the kernels to make viewing Core easier
import Test.Folderol.Kernel.Filter1
import Test.Folderol.Kernel.Map1
import Test.Folderol.Kernel.Map2
import Test.Folderol.Kernel.Map2Ignorant
import Test.Folderol.Kernel.Zip1
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


prop_zip1 :: Property
prop_zip1 = property $ do
  xs <- forAll genVec
  ys <- fmap show <$> forAll genVec
  zs <- lift $ zip1 xs ys
  zs === Vector.zip xs ys

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
tests = $$(checkSequential)
