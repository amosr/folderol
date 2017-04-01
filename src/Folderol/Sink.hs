{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
module Folderol.Sink where

import P

data Sink m a
 = forall s
 . Sink
 { init :: m s
 , push :: s -> a -> m s
 , done :: s -> m ()
 }

{-
data Sink m a
 = Sink
 { init :: m ()
 , push :: () -> a -> m ()
 , done :: () -> m ()
 }
-}

instance Monad m => Monoid (Sink m a) where
 mempty = Sink
  { init = return ()
  , push = \() _ -> return ()
  , done = \()   -> return () }

{-
 mappend (Sink init0 push0 done0) (Sink init1 push1 done1) = Sink
  { init = do
      s0 <- init0
      s1 <- init1
      return ()
  , push = \() a -> do
      s0' <- push0 () a
      s1' <- push1 () a
      return ()
  , done = \() -> do
      done0 ()
      done1 ()
  }
-}

 mappend (Sink init0 push0 done0) (Sink init1 push1 done1) = Sink
  { init = do
      s0 <- init0
      s1 <- init1
      return (s0, s1)
  , push = \(s0, s1) a -> do
      s0' <- push0 s0 a
      s1' <- push1 s1 a
      return (s0', s1')
  , done = \(s0, s1) -> do
      done0 s0
      done1 s1
  }
