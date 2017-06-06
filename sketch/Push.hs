-- Push-streams
-- Good fusion
-- Requires contra-map
module Push (xs) where

newtype Stream a = Stream ( a -> IO () )

source :: [a] -> Stream a -> IO ()
source xs (Stream k) =
  let go [] = return ()
      go (x:xs) = do
              k x
              go xs
      {-# INLINE go #-}
  in go xs
 where
{-# INLINE source #-}

zcomap :: (a -> b) -> Stream b -> Stream a
zcomap f (Stream s) = Stream $ \v -> s (f v)
{-# INLINE zcomap #-}

zfilter :: (a -> Bool) -> Stream a -> Stream a
zfilter f (Stream s) = Stream $ \v -> go v
 where
  go a
   | f a
   = s a
   | otherwise
   = return ()
{-# INLINE zfilter #-}

zprint :: Show a => Stream a
zprint = Stream $ \v -> print v
{-# INLINE zprint #-}

xs = source [1 :: Int, 2, 3] $ zcomap (+1) $ zfilter (>2) $ zprint

