module Pull  where

import Data.IORef

newtype Stream a = Stream ( IO (Maybe a) )

source :: [a] -> IO (Stream a)
source xs = do
  ref <- newIORef xs
  return $ Stream $ go ref
 where
  go ref = do
   ls <- readIORef ref
   case ls of
    [] -> return Nothing
    (l:ls') -> do
      writeIORef ref ls'
      return (Just l)
{-# INLINE source #-}


zappend :: Stream a -> Stream a -> Stream a
zappend (Stream s1) (Stream s2) = Stream go1
 where
  go1 = do
   a <- s1
   case a of
    Nothing -> s2
    Just _  -> return a

zzip :: Stream a -> Stream b -> Stream (a,b)
zzip (Stream s1) (Stream s2) = Stream go
 where 
  go = do
   a <- s1
   b <- s2
   case (a,b) of
    (Just a', Just b') -> return (Just (a',b'))
    (_      , _      ) -> return Nothing

zmap :: (a -> b) -> Stream a -> Stream b
zmap f (Stream s) = Stream ( fmap (fmap f) s)
{-# INLINE zmap #-}
