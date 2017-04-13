{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
module Folderol.Source where

import P

data Source m a
 = forall s
 . Source
 { init :: m s
 , pull :: s -> m (Maybe a, s)
 , done :: s -> m ()
 }

sourceRepeat :: Monad m => Maybe a -> Source m a
sourceRepeat a
 = Source 
 { init = return ()
 , pull = \() -> return (a, ())
 , done = \() -> return ()
 }

sourceOfList :: Monad m => [a] -> Source m a
sourceOfList as0
 = Source 
 { init = return as0
 , pull = \as -> case as of
    []      -> return (Nothing, [])
    (a:as') -> return (Just a, as')
 , done = \_  -> return ()
 }

