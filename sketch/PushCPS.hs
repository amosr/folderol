-- CPS-style push streams.
-- This corresponds to CoSrc in "On the Duality" and Push in "Expressive and Efficient Streaming Libraries".
--
-- I don't know how to implement unzip or zip for this one.
-- I can implement append, but that might be a coincidence.
module PushCPS (simple_computation, zip_computation) where

import Data.IORef

newtype Stream a = Stream ( (a -> IO ()) -> IO () )

source :: [a] -> Stream a
source xs = Stream $ \k ->
  let go [] = return ()
      go (x:xs) = do
              k x
              go xs
      {-# INLINE go #-}
  in go xs
 where
{-# INLINE source #-}

zappend :: Stream a -> Stream a -> Stream a
zappend (Stream k1) (Stream k2) = Stream $ \k -> do
  k1 k
  k2 k

zzip :: Stream a -> Stream b -> Stream (a,b)
zzip (Stream s1) (Stream s2) = error "Impossible?"


zcrossproduct :: Stream a -> Stream b -> Stream (a,b)
zcrossproduct (Stream s1) (Stream s2) = Stream $ \k -> go1 k
 where 
  go1 k     = s1 (go2 k)
  go2 k a   = s2 (go3 k a)
  go3 k a b = k  (a,b)


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
 zprint $ zzip (source [1::Int,2,3])$  zfilter (>2) $ zmap (+1) $ source [1 :: Int,2,3]

