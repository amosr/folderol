{-# LANGUAGE BangPatterns #-}
module Bench.Chan.Unagi where

import Bench.Chan.Chunk

import Folderol.Spawn
import Control.Concurrent.Chan.Unagi
import qualified Data.Vector as Vector

runMaybe :: Vector.Vector a -> IO ()
runMaybe v = do
  (i,o) <- newChan
  join2 (pushMaybe v i) (pullMaybe o)


pushMaybe :: Vector.Vector a -> InChan (Maybe a) -> IO ()
pushMaybe !v c = go 0
 where
  go i
   | i < Vector.length v
   = writeChan c (Just $ Vector.unsafeIndex v i) >> go (i+1)
   | otherwise
   = writeChan c Nothing

pullMaybe :: OutChan (Maybe a) -> IO ()
pullMaybe c = go
 where
  go = do
    v <- readChan c
    case v of
     Nothing -> return ()
     Just _  -> go


runChunked :: Int -> Vector.Vector a -> IO ()
runChunked chunkSize v = do
  (i,o) <- newChan
  join2 (pushChunk chunkSize v $ writeChan i)
        (pullChunk (\_ -> return ()) $ readChan o)

runChunkedUnbox :: Int -> Vector.Vector Int -> IO ()
runChunkedUnbox chunkSize v = do
  (i,o) <- newChan
  join2 (pushChunkUnbox chunkSize v $ writeChan i)
        (pullChunkUnbox (\_ -> return ()) $ readChan o)

