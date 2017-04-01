{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
module Folderol.Source where

import P

data Source m a
 = Source
 { init :: m ()
 , pull :: () -> m (Maybe a, ())
 , done :: () -> m ()
 }

{-
data Source m a
 = forall s
 . Source
 { init :: m s
 , pull :: s -> m (Maybe a, s)
 , done :: s -> m ()
 }
-}
