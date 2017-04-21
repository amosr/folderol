{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
module Folderol.Sink where

import P

import System.IO (IO, putStrLn)
import Data.IORef

import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector


data Sink m a
 = forall s
 . Sink
 { init :: m s
 , push :: s -> a -> m s
 , done :: s -> m ()
 }

{-# INLINE sinkPrint #-}
sinkPrint :: Show a => [Char] -> Sink IO a
sinkPrint prefix
 = Sink 
 { init = return ()
 , push = \() v -> putStrLn (prefix <> ": " <> show v)
 , done = \() -> return ()
 }

{-# INLINE listOfChannel #-}
listOfChannel :: IORef [a] -> Sink IO a
listOfChannel into
 = Sink
 { init = return []
 , push = \xs x -> return (x : xs)
 , done = \xs   -> writeIORef into (reverse xs)
 }

{-# INLINE vectorOfChannel #-}
vectorOfChannel :: IORef (Vector.Vector a) -> Sink IO a
vectorOfChannel into
 = Sink
 { init = (,) 0 <$> MVector.new 4

 , push = \(used,xs) x -> do
          xs' <- if used >= MVector.length xs
                 then MVector.grow xs (used * 2)
                 else return xs
          MVector.unsafeWrite xs' used x
          return (used + 1, xs')

 , done = \(used,xs) -> do
          xs' <- Vector.unsafeFreeze $ MVector.unsafeSlice 0 used xs
          writeIORef into xs'
 }


instance Monad m => Monoid (Sink m a) where
 mempty = Sink
  { init = return ()
  , push = \() _ -> return ()
  , done = \()   -> return () }

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
