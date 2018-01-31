{-# LANGUAGE BangPatterns #-}
module Bench.ManyChan.Unagi where

import Folderol.Spawn
import Control.Concurrent.Chan.Unagi

import qualified Data.Vector.Unboxed as Unboxed
-- import qualified Data.Vector.Unboxed.Mutable as MUnboxed

runChunkedUnbox :: Int -> Int -> Int -> IO ()
runChunkedUnbox upto chans chunkSize = do
  (i,o) <- newChan
  join2 (pushIota i 0)
        (go (chans - 1) o)
 where
  go 0 c = pullSum c 0

  go n c = do
    (i',o') <- newChan
    join2 (forward c i')
          (go (n - 1) o')

  forward from into = do
    v <- readChan from
    -- Make sure to copy the array!
    -- Because a real example would have to copy the messages one at a time.
    -- Maybe a better example would be map (+1)...
    let !v' = Unboxed.map (+1) v
    writeChan into v'
    case Unboxed.null v of
     True -> return ()
     False -> forward from into
    
  pushIota into !i
   | i < upto = do
      let !ch = upto - i `min` chunkSize
      let !v = Unboxed.generate ch (+i)
      writeChan into v
      pushIota into (i + ch)
   | otherwise = do
      writeChan into Unboxed.empty

  pullSum from !xxsum = do
    v <- readChan from
    case Unboxed.null v of
     True -> if xxsum < 0
             -- Do something stupid with the sum at the end to make sure it sticks around
             then fail "Error: sum is negative. It shouldn't be"
             else return ()
     False -> do
      pullSum from (xxsum + Unboxed.sum v)

