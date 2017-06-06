module Pull (xs) where

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
zzip (Stream k1) (Stream k2) = Stream $ \k -> go1 k
 where 
  go1 k     = k1 (go2 k)
  go2 k a   = k2 (go3 k a)
  go3 k a b = do
    k (a,b)
    go1 k

zzip2 :: Stream a -> Stream b -> Stream (a,b)
zzip2 (Stream k1) (Stream k2) = Stream (\k ->
  let go1     = k1 go2
      go2 a   = k2 (go3 a)
      go3 a b = do
        k (a, b)
        go1
  in  go1)

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

xs = zprint $ zfilter (>2) $ zmap (+1) $ source [1 :: Int,2,3]

