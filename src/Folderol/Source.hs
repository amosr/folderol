{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
module Folderol.Source where

import P

import qualified Data.Vector.Generic as Generic

import qualified Control.Monad.Morph as Morph

data Source m a
 = forall s
 . Source
 { init :: m s
 , pull :: s -> m (Maybe a, s)
 , done :: s -> m ()
 }

instance Morph.MFunctor Source where
 hoist f (Source i p d) = Source (f i) (f . p) (f . d)

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
sourceOfVector :: (Monad m, Generic.Vector v a) => v a -> Source m a
sourceOfVector !as0
 = Source 
 { init = return 0
 , pull = \ix -> if ix >= Generic.length as0
                 then return (Nothing, ix)
                 else return (Just $ Generic.unsafeIndex as0 ix, ix + 1)
 , done = \_  -> return ()
 }

