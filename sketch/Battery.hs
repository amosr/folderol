-- Continuation passing-style pull streams, except the stream calls the result continuation many times - once for each value.
--
-- Recall the definitions of push and pull:
-- > Pull a = IO a
-- > Push a = a -> IO ()
--
-- If we take a Pull stream and convert it to CPS, we get CoPull:
-- > CoPull a = ((a -> IO ()) -> IO ())
-- (Assume we don't really care about the result of the continuation, so we just replace it with unit.)
-- This is equivalent to a push stream whose elements are continuations:
-- >   CoPull a
-- > = Push (N a)
-- > = Push (a -> IO ())
-- > = Push (Push a)
--
-- We could take this CoPull and just use it as a regular pull stream which supports zip (see CoPull.hs).
-- But we can also do something crazy and call the continuation many times.
-- If we want to produce the list [1, 2, 3], we can write it as:
-- > list123 = CoPull $ \k -> do
-- >    k 1
-- >    k 2
-- >    k 3
--
-- It turns out this convinces the Java JIT to produce good code, but the downside is that this has none of the good parts of either Push or Pull: straight lines only, no zips, no unzips, no nothing.
--
-- It supports append by passing the same continuation to both input streams. "Defunctionalising Push Arrays" (Svensson & Svenningsson)
-- You can express an index-based pull array as (Ix -> a), but appending two of these gives code something like
-- > a ++ b = \ix. if ix < length a then a ix else b (ix - length a)
-- and this is bad because every iteration has an extra bounds check.
-- If you express your array as an "index-based push array" ((Ix -> a -> IO ()) -> IO ()), then you can append them better:
-- > a ++ b = \k. a k; b (\ix. k (ix + length a))
--
-- This corresponds to the push arrays from Obsidian, and other places.
-- According to "Expressive and Efficient Streaming Libraries" (Biboudis), this is what Java 8 Streams uses.
-- In (Biboudis) they are called push streams, but I think this is a bad name.
-- I need to look in the references to see if anybody else has a more descriptive name for this.
-- I'm proposing to call them "Battery streams" because the continuation fires many times.
--
-- We can also define Co-push:
-- >   CoPush a
-- > = Pull (Push a)
-- > = IO (a -> IO ())
-- as well as maybe PushPull and PullPull:
-- > PushPull a = Push (Pull a) = (IO a -> IO ()) -> IO ()
-- > PullPull a = Pull (Pull a) = IO (IO a)
-- and because these all have some sort of nested structure, I'm assuming they have a mean different things depending on whether the inner part can be fired many times or not.
-- So I guess we could have Battery CoPush etc.
module Battery (simple_computation, zip_computation) where

import Data.IORef
import Control.Concurrent

newtype Stream a = Stream ( (a -> IO ()) -> IO () )

source :: [a] -> Stream a
source xs = Stream $ \k ->
  -- Once we have the continuation, we go through the whole list and push every element into it.
  -- One continuation gets all the values.
  let go [] = return ()
      go (x:xs) = do
              k x
              go xs
      {-# INLINE go #-}
  in go xs
 where
{-# INLINE source #-}

-- This definition of append is pretty nice: run through all of first input, then all of second input.
-- Compare this to Pull or CoPull, where every time the consumer asks for a value we need to check whether we've reached the end of the first input or not.
-- Here we don't need any checks.
-- It would be a little more complicated if we used Maybe to denote the end, but still doable.
zappend :: Stream a -> Stream a -> Stream a
zappend (Stream k1) (Stream k2) = Stream $ \k -> do
  k1 k
  k2 k

-- If we take the zip definition from CoPull, we end up computing the cross product!
-- Inside s1's continuation we call s2.
-- Because s1 calls its continuation many times, we end up calling s2 once for each value in s1.
-- Then s2 calls its continuation many times, so we end up calling the final continuation "k" m*n times
zzipCoPull :: Stream a -> Stream b -> (Stream (a,b))
zzipCoPull (Stream s1) (Stream s2) = do
  Stream $ \k -> do
    s1 $ \a ->
      s2 $ \b ->
        k (a, b)

-- (Biboudis) "One could emulate zip using iterator from push-streams—at significant drop in performance."
-- This is Java 8 Stream specific: Java 8 Streams seem to be implemented by building up both the battery representation and pull representation, which means they can be executed as battery (fast) or converted to pull (slow, not suited to Java JIT).
-- I can't see how to convert this version to Pull without introducing threading or storing the whole stream in an array.
-- Using threading introduces a lot of overhead and is not going to fuse.
--
-- If we ran 'pullOf' from CoPull.hs, it would just get the last value of the stream.
-- (Actually since CoPull always puts a Nothing at the end, it would just return the Nothing)
pullOf :: Stream a -> IO (IO a)
pullOf (Stream s) = do
  mvar   <- newEmptyMVar
  forkIO (s $ putMVar mvar)
  return $ takeMVar mvar


zmap :: (a -> b) -> Stream a -> Stream b
zmap f (Stream s) = Stream $ \k -> s (k . f)
{-# INLINE zmap #-}

zfilter :: (a -> Bool) -> Stream a -> Stream a
zfilter f (Stream s) = Stream $ \k -> s (go k)
 where
  go k a
   | f a
   = k a
   | otherwise
   = return ()
{-# INLINE zfilter #-}

zprint :: Show a => Stream a -> IO ()
zprint (Stream s) = s print
{-# INLINE zprint #-}

simple_computation = zprint $  zfilter (>2) $ zmap (+1) $ source [1 :: Int,2,3]

zip_computation = do
 zprint $ zzipCoPull (source [1::Int,2,3])$  zfilter (>2) $ zmap (+1) $ source [1 :: Int,2,3]

