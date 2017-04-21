{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
module Folderol.Source where

import P

import qualified Data.Vector as Vector

data Source m a
 = forall s
 . Source
 { init :: m s
 , pull :: s -> m (Maybe a, s)
 , done :: s -> m ()
 }

{-# INLINE sourceRepeat #-}
sourceRepeat :: Monad m => Maybe a -> Source m a
sourceRepeat a
 = Source 
 { init = return ()
 , pull = \() -> return (a, ())
 , done = \() -> return ()
 }

{-# INLINE sourceOfList #-}
sourceOfList :: Monad m => [a] -> Source m a
sourceOfList as0
 = Source 
 { init = return as0
 , pull = \as -> case as of
    []      -> return (Nothing, [])
    (a:as') -> return (Just a, as')
 , done = \_  -> return ()
 }

{-# INLINE sourceOfVector #-}
sourceOfVector :: Monad m => Vector.Vector a -> Source m a
sourceOfVector !as0
 = Source 
 { init = return 0
 , pull = \ix -> if ix < Vector.length as0
                 then return (Just $ Vector.unsafeIndex as0 ix, ix + 1)
                 else return (Nothing, ix)
 , done = \_  -> return ()
 }

