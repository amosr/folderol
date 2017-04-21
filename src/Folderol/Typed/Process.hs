{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Typed.Process (
  proc
 , input
 , output

 , pull
 , push
 , drop
 , jump
 , bool
 , done

 , label0
 , label1
 , label2
 , label3
 , label4
 , label5

 , instr0
 , instr1
 , instr2
 , instr3
 , instr4
 , instr5

 , Process
 , Input
 ) where

import Folderol.Typed.Name
import Folderol.Typed.Network
import Folderol.Typed.Process.Internal

import qualified Folderol.Untyped.Name as U
import qualified Folderol.Untyped.Network as U
import qualified Folderol.Untyped.Process as U

import P hiding (drop, bool)

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Control.Monad.Trans.State as State

import qualified Folderol.Internal.Haskell as Haskell

import Data.String (String)

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


