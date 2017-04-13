{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Typed.Process where

import Folderol.Typed.Name
import Folderol.Typed.Network

import qualified Folderol.Untyped.Name as U
import qualified Folderol.Untyped.Network as U
import qualified Folderol.Untyped.Process as U

import P

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Monad.Trans.Class (MonadTrans(..))
import qualified Control.Monad.Trans.State as State

import qualified Folderol.Internal.Haskell as Haskell

import Data.String (String)

import qualified Prelude as Savage

data ProcessInfo
 = ProcessInfo
 { piInputs       :: Map U.Channel [U.Channel]
 , piOutputs      :: Set U.Channel
 , piInstructions :: Map U.Label U.Info
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

proc :: Monad m => String -> Process m (LabelRef, a) -> Network m a
proc name p = do
  ((init, r), pis) <- State.runStateT (runProcess p name) (ProcessInfo Map.empty Set.empty Map.empty)
  -- TODO: insert dups
  let overs = Map.filter ((>1) . length) (piInputs pis)
  when (not $ Map.null overs) $
    fail "Unsupported (yet): process consuming same stream multiple times, eg 'zip xs xs' or 'append xs xs'."

  i0 <- U.liftQ init
  let p0 = U.Process name (Map.keysSet $ piInputs pis) (piOutputs pis) i0 (piInstructions pis)
  U.tell $ U.NetworkGraph Map.empty Map.empty [p0]
  return r


procQ :: Monad m => Haskell.Q a -> State.StateT s (Network m) a
procQ m = lift (U.liftQ m)

sfresh :: Monad m => String -> String -> State.StateT a (Network m) Haskell.Name
sfresh pre name = lift $ U.liftQ $ Haskell.newName (pre <> "_" <> name)

input :: Monad m => Channel a -> Process m (Input a)
input (UnsafeChannel u) = Process $ \name -> do
  s <- State.get
  (u',us') <- go name $ Map.lookup u $ piInputs s
  State.put s { piInputs = Map.insert u (u':us') $ piInputs s }
  return (UnsafeInput u')
 where
  go _ Nothing = return (u, [])
  go name (Just us) = do
    u' <- U.Channel <$> sfresh name "dup"
    return (u', us)

output :: Monad m => Process m (Channel a)
output = Process $ \name -> do
  s <- State.get
  u' <- U.Channel <$> sfresh name "out"
  State.put s { piOutputs = Set.insert u' $ piOutputs s }
  return $ UnsafeChannel u'


type LabelRef = Haskell.Q U.Next
type InstructionRef = Haskell.Q U.Instruction

label0 :: Monad m => Process m LabelRef
label0 = Process $ \name -> do
  l <- U.Label <$> sfresh name "label"
  return $ return $ U.Next l Map.empty

instr0 :: Monad m => LabelRef -> InstructionRef -> Process m ()
instr0 lr ir = Process $ \_ -> do
  U.Next l _ <- procQ lr
  i <- procQ ir
  s <- State.get
  State.put s { piInstructions = Map.insert l (U.Info Set.empty i) $ piInstructions s }


label1 :: Monad m => Process m (Haskell.TExpQ a -> LabelRef)
label1 = Process $ \name -> do
  l <- U.Label <$> sfresh name "label"
  v <- U.Var <$> sfresh name "var"
  return $ \x -> do
    x' <- x
    return $ U.Next l $ Map.singleton v (Haskell.unType x')

instr1 :: Monad m => (Haskell.TExpQ a -> LabelRef) -> (Haskell.TExpQ a -> InstructionRef) -> Process m ()
instr1 lr ir = Process $ \_ -> do
  U.Next l is <- procQ $ lr [|| Savage.error "Placeholder variable" ||]
  let binds = Map.keysSet is
  let [U.Var b0] = Map.keys is
  i <- procQ $ ir (Haskell.unsafeTExpCoerce $ Haskell.varE b0)
  s <- State.get
  State.put s { piInstructions = Map.insert l (U.Info binds i) $ piInstructions s }


pull :: Input a -> (Haskell.TExpQ a -> LabelRef) -> LabelRef -> InstructionRef
pull is some0 none0 = do
  magic <- Haskell.newName "magic"
  U.Next lsome usome <- some0 (Haskell.unsafeTExpCoerce $ Haskell.varE magic)
  let [k] = Map.keys $ Map.filter (==Haskell.VarE magic) usome
  let usome' = Map.delete k usome
  let nsome = U.Next lsome usome'
  nnone <- none0
  return $ U.I'Pull (unInput is) k nsome nnone

push :: Channel a -> Haskell.TExpQ a -> LabelRef -> InstructionRef
push os e0 next0 = do
  next <- next0
  e    <- Haskell.unType <$> e0 
  return $ U.I'Push (unChannel os) e next

drop :: Input a -> LabelRef -> InstructionRef
drop is next0 = do
  next <- next0
  return $ U.I'Drop (unInput is) next

jump :: LabelRef -> InstructionRef
jump next0 = do
  next <- next0
  return $ U.I'Jump  next

bool :: Haskell.TExpQ Bool -> LabelRef -> LabelRef -> InstructionRef
bool b0 true0 false0 = do
  b      <- Haskell.unType <$> b0
  ntrue  <- true0
  nfalse <- false0
  return $ U.I'Bool b ntrue nfalse

done :: InstructionRef
done = return $ U.I'Done

