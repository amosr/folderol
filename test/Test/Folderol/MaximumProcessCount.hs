{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.MaximumProcessCount where

import Folderol
import Folderol.Splice
import P

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Vector as Vector

import System.IO
import           Control.Monad.Trans.Class (MonadTrans(..))

{-# NOINLINE map2 #-}
map2 :: (a -> b) -> (b -> c) -> Vector.Vector a -> IO (Vector.Vector c)
map2 f g =
 $$(fuseVector_1_1 defaultFuseOptions { maximumProcessCount = Just 1 } $ \as -> do
  bs <- map [||f||] as
  cs <- map [||g||] bs
  return cs)

genVec :: Gen IO (Vector.Vector Int)
genVec = Vector.fromList <$> Gen.list (Range.linear 0 100) (Gen.int $ Range.linear 0 100)

prop_increment_2 :: Property
prop_increment_2 = property $ do
  xs <- forAll genVec
  ys <- lift $ map2 (+1) (*2) xs
  ys === fmap (*2) (fmap (+1) xs)

tests :: IO Bool
tests = $$(checkSequential)
