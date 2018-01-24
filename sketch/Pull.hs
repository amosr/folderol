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


-- mux from On the Duality: consumer's choice which one to pull from
-- in OtD this has type
--  mux :: CoSrc a → CoSrc b → CoSrc (a & b)
-- and cannot be implemented on their Src definition.
-- This is because a Src returns two separate values: the element and the remainder of the stream.
-- The remainder of the stream cannot depend on what the environment does with the element, so the Src implementation must choose which one to pull from beforehand.
--
-- We are cheating by putting the remainder stream in IO so we can repeatedly pull from it, so it's not a direct comparison, but whatever.
mux :: Stream a -> Stream b -> Stream (Either (Maybe a -> IO ()) (Maybe b -> IO ()) -> IO ())
mux (Stream sa) (Stream sb) = Stream $ return $ Just $ \request ->
  case request of
   Left with -> do
    a <- sa
    with a
   Right with -> do
    b <- sb
    with b
  

