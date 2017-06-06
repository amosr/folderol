{-# LANGUAGE GADTs #-}
module Coroutine where

import Data.IORef
import Control.Monad.State

import Control.Concurrent

data Proc a where
  Wait :: Proc a -> Proc a
  Lift :: IO a -> (a -> Proc b) -> Proc b
  Done :: a -> Proc a

instance Functor Proc where
 fmap f a = case a of
  Wait r -> f <$> r
  Lift e r -> Lift e (\c -> f <$> r c)
  Done v -> Done $ f v

instance Applicative Proc where
 pure = Done
 a <*> b = case a of
  Wait r   -> Wait (r <*> b)
  Lift e r -> Lift e (\c -> r c <*> b)
  Done v   -> v <$> b
 
instance Monad Proc where
 a >>= b = case a of
  Wait r -> Wait (r >>= b)
  Lift f g -> Lift f (\c -> g c >>= b)
  Done v -> b v

instance MonadIO Proc where
 liftIO f = Lift f Done

data ChannelBuffer a
 = ChannelHave a
 | ChannelWait
 | ChannelOver
 deriving (Eq, Ord, Show)

data Input a = Input (IORef (ChannelBuffer a))

data Channel a = Channel (IORef [IORef (ChannelBuffer a)])

pull :: Input a -> Proc (Maybe a)
pull i@(Input io) = do
  c <- liftIO $ readIORef io
  case c of
   ChannelHave a -> do
    liftIO $ writeIORef io ChannelWait
    return $ Just a
   ChannelOver ->
    return Nothing
   ChannelWait ->
    Wait $ pull i

push :: Maybe a -> Channel a -> Proc ()
push v0 (Channel ls) = do
  ls' <- liftIO $ readIORef ls
  go ls'
 where
  v = case v0 of
   Nothing -> ChannelOver
   Just v' -> ChannelHave v'

  go [] = return ()
  go (i:is) = do
   c <- liftIO $ readIORef i
   case c of
    ChannelWait -> do
     liftIO $ writeIORef i v
     go is
    ChannelOver -> go is
    ChannelHave _ -> do
     Wait (go (i:is))


type Graph = StateT [(String, Proc ())] IO

reader :: Channel a -> Graph (Input a)
reader (Channel os) = do
  c <- liftIO $ newIORef ChannelWait
  liftIO $ modifyIORef os (c:)
  return $ Input c

output :: Graph (Channel a)
output = do
  c <- liftIO $ newIORef []
  return $ Channel c

proc :: String -> Proc () -> Graph ()
proc s p = modify ((s,p):)

runGraph :: Graph () -> IO ()
runGraph g0 = do
  ps <- execStateT g0 []
  go ps
 where
  go [] = return ()
  go ((s,p):ps) = do
   threadDelay 10000
   p' <- unwrap1 p
   case p' of
    Done () -> go ps
    _ -> go (ps ++ [(s,p')])

  unwrap1 p = case p of
   Wait r -> run1 r
   _      -> run1 p

  run1 p = case p of
   Wait r -> return $ Wait r
   Lift e r -> do
    c <- e
    run1 (r c)
   Done v -> return $ Done v


pmap :: (a -> b) -> Channel a -> Graph (Channel b)
pmap f as = do
  as' <- reader as
  bs  <- output
  proc "map" $ prun as' bs
  return bs
 where
  prun is os = do
   a <- pull is
   push (f <$> a) os
   case a of
    Nothing -> return ()
    Just _  -> prun is os

piota :: Int -> Graph (Channel Int)
piota iMax = do
  bs <- output
  proc "iota" $ prun 0 bs
  return bs
 where
  prun i bs
   | i == iMax
   = do push Nothing bs
        return ()
   | otherwise
   = do push (Just i) bs
        prun (i + 1) bs

pprint :: Show a => String -> Channel a -> Graph ()
pprint str as = do
  is <- reader as
  proc "print" $ prun is
 where
  prun is = do
   a <- pull is
   case a of
    Nothing -> return ()
    Just a' -> do
      liftIO $ putStrLn (str ++ show a')
      prun is

pappend :: Channel a -> Channel a -> Graph (Channel a)
pappend as bs = do
  as' <- reader as
  bs' <- reader bs
  os  <- output
  proc "append" $ (pdrain as' os >> pdrain bs' os >> push Nothing os)
  return os
 where
  pdrain i o = do
    v <- pull i
    case v of
     Nothing -> return ()
     Just _  -> do
      push v o
      pdrain i o


eg_map2 :: Graph ()
eg_map2 = do
  a  <- piota 10
  b  <- pmap (+1) a
  c  <- pmap (*2) b
  pprint "c: " c

eg_app :: Graph ()
eg_app = do
  a1  <- piota 10
  a1' <- pmap Left a1
  a2  <- piota 10
  a2' <- pmap Right a2
  os  <- pappend a1' a2'
  pprint "" os

eg_app_deadlock :: Graph ()
eg_app_deadlock = do
  a1  <- piota 10
  os  <- pappend a1 a1
  pprint "" os

