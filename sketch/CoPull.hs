-- Continuation passing-style pull streams, which are basically just a more complicated kind of pull stream.
-- This corresponds to CoSrc in "On the Duality" (Bernardy, Svenningsson)
--
-- >   CoPull a
-- > = Push (N a)
-- > = Push (a -> IO ())
-- > = Push (Push a)
--
module CoPull (simple_computation, zip_computation) where

import Data.IORef

newtype Stream a = Stream { unStream :: ( (Maybe a -> IO ()) -> IO () ) }

source :: [a] -> IO (Stream a)
source xs0 = do
  xsR <- newIORef xs0
  return $ Stream $ \k -> do
    xs <- readIORef xsR
    case xs of
     [] -> k Nothing
     (x:xs') -> do
      writeIORef xsR xs'
      k (Just x)
{-# INLINE source #-}

zappend :: Stream a -> Stream a -> Stream a
zappend (Stream k1) (Stream k2) = Stream $ \k -> 
  k1 $ \a ->
    case a of
     Nothing -> k2 k
     Just a -> k (Just a)

zzip :: Stream a -> Stream b -> IO (Stream (a,b))
zzip (Stream s1) (Stream s2) = do
  return $ Stream $ \k -> do
    s1 $ \a ->
      s2 $ \b ->
        k ((,) <$> a <*> b)

-- This would be even simpler if the continuation were polymorphic in the return type:
-- > pullOf (Stream s) = s return
pullOf :: Stream a -> IO (Maybe a)
pullOf (Stream s) = do
  ref <- newIORef Nothing
  s (writeIORef ref)
  readIORef ref

zmap :: (a -> b) -> Stream a -> Stream b
zmap f (Stream s) = Stream $ \k -> s (k . fmap f)
{-# INLINE zmap #-}

zfilter :: (a -> Bool) -> Stream a -> Stream a
zfilter f (Stream s) = Stream $ \k -> s (go k)
 where
  go k Nothing
   = k Nothing
  go k (Just a)
   | f a
   = k (Just a)
   | otherwise
   = s (go k)
{-# INLINE zfilter #-}

zprint :: Show a => Stream a -> IO ()
zprint (Stream s) = s go
 where
  go Nothing = return ()
  go (Just a) = do
    print a
    s go
{-# INLINE zprint #-}

simple_computation = do
  s <- source [1 :: Int,2,3]
  zprint $  zfilter (>2) $ zmap (+1) s

zip_computation = do
  s <- source [1 :: Int, 2, 3]
  s2 <- source [4 :: Int, 5, 6]
  zz <- zzip s2 s -- $  zfilter (>2) $ zmap (+1) s
  zprint zz

