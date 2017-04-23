{-# LANGUAGE BangPatterns #-}
module Bench.Chan.Base where

import Bench.Chan.Chunk

import Folderol.Spawn
import Control.Concurrent
import qualified Data.Vector as Vector

runMaybe :: Vector.Vector a -> IO ()
runMaybe v = do
  c <- newChan
  join2 (pushMaybe v c) (pullMaybe c)


pushMaybe :: Vector.Vector a -> Chan (Maybe a) -> IO ()
pushMaybe !v c = go 0
 where
  go i
   | i < Vector.length v
   = writeChan c (Just $ Vector.unsafeIndex v i) >> go (i+1)
   | otherwise
   = writeChan c Nothing

pullMaybe :: Chan (Maybe a) -> IO ()
pullMaybe c = go
 where
  go = do
    v <- readChan c
    case v of
     Nothing -> return ()
     Just _  -> go


runChunked :: Int -> Vector.Vector a -> IO ()
runChunked chunkSize v = do
  c <- newChan
  join2 (pushChunk chunkSize v $ writeChan c)
        (pullChunk (\_ -> return ()) $ readChan c)

runChunkedUnbox :: Int -> Vector.Vector Int -> IO ()
runChunkedUnbox chunkSize v = do
  c <- newChan
  join2 (pushChunkUnbox chunkSize v $ writeChan c)
        (pullChunkUnbox (\_ -> return ()) $ readChan c)

