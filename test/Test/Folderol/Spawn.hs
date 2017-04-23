{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.Spawn where

import qualified Folderol.Spawn as Spawn
import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import P

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.IORef
import System.IO
import           Control.Monad.Trans.Class (MonadTrans(..))


prop_chan :: Property
prop_chan = property $ do
  -- Make sure to generate a few multiples of the chunk size,
  -- so we go through a full fill / send cycle
  xs <- forAll $ Gen.list (Range.linear 0 $ Spawn.channelChunkSize * 3) (Gen.int $ Range.linear 0 100)

  xs' <- lift $ do
    (sink,source) <- Spawn.channel
    ref <- newIORef []
    Spawn.join2 (from sink xs) (into source ref)
    readIORef ref

  xs === xs'

 where
  from (Sink.Sink init push done) xs0
   = let go s [] = done s
         go s (x:xs) = do
            s' <- push s x
            go s' xs
     in init >>= \s -> go s xs0

  into (Source.Source init pull done) ref
   = let go xs (Nothing,s) = done s >> writeIORef ref (reverse xs)
         go xs (Just x, s) = pull s >>= go (x:xs)
     in init >>= pull >>= go []


tests :: IO Bool
tests = $$(checkConcurrent)

