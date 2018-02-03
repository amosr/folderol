{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Typed.Process.Internal where

import Folderol.Typed.Network

import qualified Folderol.Untyped.Name as U
import qualified Folderol.Untyped.Network as U
import qualified Folderol.Untyped.Process as U

import P

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)

import           Control.Monad.Trans.Class (MonadTrans(..))
import qualified Control.Monad.Trans.State as State

import qualified Folderol.Internal.Haskell as Haskell

import Data.String (String)

import qualified Prelude

-- Process monad ---------------
data ProcessInfo
 = ProcessInfo
 { piInputs       :: Map U.Channel [U.Channel]
 , piOutputs      :: Set U.Channel
 , piInstructions :: Map U.Label U.Info
 , piLabelCount   :: Int
 }

data Input a
 = UnsafeInput { unInput :: U.Channel }

newtype Process m a
 = Process
 { runProcess :: String -> State.StateT ProcessInfo (Network m) a }

instance Monad m => Functor (Process m) where
 fmap f m
  = Process $ \name -> do
      a <- runProcess m name
      return $ f a

instance Monad m => Applicative (Process m) where
 pure a = Process $ \_ -> return a
 (<*>) u v = Process $ \name -> do
      f <- runProcess u name
      a <- runProcess v name
      pure (f a)

instance Monad m => Monad (Process m) where
 (>>=) u v = Process $ \name -> do
      a <- runProcess  u    name
      runProcess (v a) name

procQ :: Monad m => Haskell.Q a -> State.StateT s (Network m) a
procQ m = lift (U.liftQ m)

sfresh :: Monad m => String -> String -> State.StateT a (Network m) Haskell.Name
sfresh pre name = lift $ U.liftQ $ Haskell.newName (pre <> "_" <> name)

pfresh :: Monad m => String -> Process m Haskell.Name
pfresh name = Process $ \pre -> lift $ U.liftQ $ Haskell.newName (pre <> "_" <> name)

lfresh :: Monad m => Process m Haskell.Name
lfresh = Process $ \pre -> do
  s <- State.get
  let c = piLabelCount s
  l <- lift $ U.liftQ $ Haskell.newName (pre <> "_L" <> show c)
  State.put s { piLabelCount = c + 1 }
  return l


-- Generating labels ---------------
type LabelRef = Haskell.Q U.Next

labelZ :: Monad m => Process m (Map U.Var Haskell.Exp -> () -> LabelRef)
labelZ = do
  l <- U.Label <$> lfresh
  return $ \m _ -> return $ U.Next l m

labelS :: Monad m => Process m (Map U.Var Haskell.Exp -> l -> LabelRef) -> Process m (Map U.Var Haskell.Exp -> (Haskell.TExpQ a, l) -> LabelRef)
labelS f = do
  f' <- f
  v <- U.Var <$> pfresh "var"
  return $ \m (x,l) -> do
    x' <- x
    f' (Map.insert v (Haskell.unType x') m) l

labelF :: Monad m => ((l -> LabelRef) -> i) -> Process m (Map U.Var Haskell.Exp -> l -> LabelRef) -> Process m i
labelF pre f = do
  f' <- f
  return $ pre $ f' Map.empty


label0 :: Monad m => Process m LabelRef
label0 =
  labelF (\f -> f ()) $ labelZ

label1 :: Monad m => Process m (Haskell.TExpQ a -> LabelRef)
label1 =
  labelF (\f x -> f (x,())) $ labelS $ labelZ

label2 :: Monad m => Process m (Haskell.TExpQ a -> Haskell.TExpQ b -> LabelRef)
label2 = 
  labelF (\f x y -> f (x,(y,()))) $ labelS $ labelS $ labelZ

label3 :: Monad m => Process m (Haskell.TExpQ a -> Haskell.TExpQ b -> Haskell.TExpQ c -> LabelRef)
label3 = 
  labelF (\f x y z -> f (x,(y,(z,())))) $ labelS $ labelS $ labelS $ labelZ

label4 :: Monad m => Process m (Haskell.TExpQ a -> Haskell.TExpQ b -> Haskell.TExpQ c -> Haskell.TExpQ d -> LabelRef)
label4 = 
  labelF (\f a b c d -> f (a,(b,(c,(d,()))))) $ labelS $ labelS $ labelS $ labelS $ labelZ

label5 :: Monad m => Process m (Haskell.TExpQ a -> Haskell.TExpQ b -> Haskell.TExpQ c -> Haskell.TExpQ d -> Haskell.TExpQ e -> LabelRef)
label5 = 
  labelF (\f a b c d e -> f (a,(b,(c,(d,(e,())))))) $ labelS $ labelS $ labelS $ labelS $ labelS $ labelZ


-- Associating label with instruction ---------------
type InstructionRef = Haskell.Q U.Instruction


instr0 :: Monad m => LabelRef -> InstructionRef -> Process m ()
instr0 lr ir = instrPut lr (const ir)

instr1 :: Monad m => (Haskell.TExpQ a -> LabelRef) -> (Haskell.TExpQ a -> InstructionRef) -> Process m ()
instr1 lr ir = instrPut
  (lr instrMagic)
  (\[e0] -> ir $ Haskell.unsafeTExpCoerce e0)

instr2 :: Monad m => (Haskell.TExpQ a -> Haskell.TExpQ b -> LabelRef) -> (Haskell.TExpQ a -> Haskell.TExpQ b -> InstructionRef) -> Process m ()
instr2 lr ir = instrPut
  (lr instrMagic instrMagic)
  (\[e0, e1] -> ir (Haskell.unsafeTExpCoerce e0) (Haskell.unsafeTExpCoerce e1))

instr3 :: Monad m => (Haskell.TExpQ a -> Haskell.TExpQ b -> Haskell.TExpQ c -> LabelRef) -> (Haskell.TExpQ a -> Haskell.TExpQ b -> Haskell.TExpQ c -> InstructionRef) -> Process m ()
instr3 lr ir = instrPut
  (lr instrMagic instrMagic instrMagic)
  (\[e0, e1, e2] -> ir (Haskell.unsafeTExpCoerce e0) (Haskell.unsafeTExpCoerce e1) (Haskell.unsafeTExpCoerce e2))

instr4 :: Monad m => (Haskell.TExpQ a -> Haskell.TExpQ b -> Haskell.TExpQ c -> Haskell.TExpQ d -> LabelRef) -> (Haskell.TExpQ a -> Haskell.TExpQ b -> Haskell.TExpQ c -> Haskell.TExpQ d -> InstructionRef) -> Process m ()
instr4 lr ir = instrPut
  (lr instrMagic instrMagic instrMagic instrMagic)
  (\[e0, e1, e2, e3] -> ir (Haskell.unsafeTExpCoerce e0) (Haskell.unsafeTExpCoerce e1) (Haskell.unsafeTExpCoerce e2) (Haskell.unsafeTExpCoerce e3))

instr5 :: Monad m => (Haskell.TExpQ a -> Haskell.TExpQ b -> Haskell.TExpQ c -> Haskell.TExpQ d -> Haskell.TExpQ e -> LabelRef) -> (Haskell.TExpQ a -> Haskell.TExpQ b -> Haskell.TExpQ c -> Haskell.TExpQ d -> Haskell.TExpQ e -> InstructionRef) -> Process m ()
instr5 lr ir = instrPut
  (lr instrMagic instrMagic instrMagic instrMagic instrMagic)
  (\[e0, e1, e2, e3, e4] -> ir (Haskell.unsafeTExpCoerce e0) (Haskell.unsafeTExpCoerce e1) (Haskell.unsafeTExpCoerce e2) (Haskell.unsafeTExpCoerce e3) (Haskell.unsafeTExpCoerce e4))

instrPut :: Monad m => LabelRef -> ([Haskell.ExpQ] -> InstructionRef) -> Process m ()
instrPut lr ir = Process $ \_ -> do
  U.Next l is <- procQ lr
  let binds = Map.keysSet is
  let exprs = reverse $ fmap (return . Haskell.VarE . U.unVar) $ Map.keys is
  i <- procQ $ ir exprs
  s <- State.get
  State.put s { piInstructions = Map.insert l (U.Info binds i) $ piInstructions s }

instrMagic :: Haskell.TExpQ a
instrMagic = [|| Prelude.error "MAGIC: instruction placeholder" ||]

