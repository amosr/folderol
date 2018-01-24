{-# LANGUAGE BangPatterns #-}
module Bench.ManyChan.Base where

import Folderol.Spawn
import Control.Concurrent

import qualified Data.Vector.Unboxed as Unboxed
-- import qualified Data.Vector.Unboxed.Mutable as MUnboxed

runChunkedUnbox :: Int -> Int -> Int -> IO ()
runChunkedUnbox upto chans chunkSize = do
  c <- newChan
  join2 (pushIota c 0)
        (go (chans - 1) c)
 where
  go 0 c = pullSum c 0

  go n c = do
    c' <- newChan
    join2 (forward c c')
          (go (n - 1) c')

  forward from into = do
    v <- readChan from
    -- Make sure to copy the array!
    -- Because a real example would have to copy the messages one at a time.
    -- Maybe a better example would be map (+1)...
    let v' = Unboxed.map (\a -> a) v
    writeChan into v'
    case Unboxed.null v of
     True -> return ()
     False -> forward from into
    
  pushIota into i
   | i < upto = do
      let ch = upto - i `min` chunkSize
      let v = Unboxed.generate ch (+i)
      writeChan into v
      pushIota into (i + ch)
   | otherwise = do
      writeChan into Unboxed.empty

  pullSum from xxsum = do
    v <- readChan from
    case Unboxed.null v of
     True -> if xxsum < 0
             -- Do something stupid with the sum at the end to make sure it sticks around
             then fail "Error: sum is negative. It shouldn't be"
             else return ()
     False -> do
      pullSum from (xxsum + Unboxed.sum v)

