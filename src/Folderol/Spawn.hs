{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
module Folderol.Spawn where

import P

import System.IO (IO)

class Monad m => Spawn m where
 join2 :: m () -> m () -> m ()

instance Spawn IO where
 join2 a b = a >> b
