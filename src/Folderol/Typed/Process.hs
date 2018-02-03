{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Typed.Process (
  proc
 , input
 , output
 , dup1

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

import qualified Folderol.Untyped.Builtins as U
import qualified Folderol.Untyped.Name as U
import qualified Folderol.Untyped.Network as U
import qualified Folderol.Untyped.Process as U

import P hiding (drop, bool, either)

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Control.Monad.Trans.State as State

import qualified Folderol.Internal.Haskell as Haskell

import Data.String (String)

proc :: Monad m => String -> Process m (LabelRef, a) -> Network m a
proc name p = do
  ((init, r), pis) <- State.runStateT (runProcess p name) (ProcessInfo Map.empty Set.empty Map.empty 0)

  -- Simplest possible way to insert duplicates:
  -- channel input is
  --
  -- > [cOriginal, cDup1, cDup2..]
  -- so insert new processes to copy from cOriginal to cDup*
  --
  -- cOriginal ->dup1-> cDup1
  -- cOriginal ->dup1-> cDup2
  --
  -- The insertDups transform will actually insert more duplicators for cOriginal later,
  -- but that's okay: they are simple enough that they should be fused away.
  pdups <- concat <$> mapM duplicate (piInputs pis)

  i0 <- U.liftQ init
  let inputs = Set.fromList $ concat $ Map.elems $ piInputs pis
  let p0 = U.Process name inputs (piOutputs pis) i0 (piInstructions pis)
  U.tell $ U.createNetwork Map.empty Map.empty (pdups <> [p0])
  return r
 where
  duplicate []
   = return []
  duplicate (c:cs)
   = mapM (U.liftQ . U.dup1Into c) cs

-- Pass-through: useless, but sometimes useless things help me debug.
dup1 :: Monad m => Channel a -> Network m (Channel a)
dup1 (UnsafeChannel c) = do
  (p,c') <- U.liftQ $ U.dup1 c
  U.tell $ U.createNetwork Map.empty Map.empty [p]
  return $ UnsafeChannel c'

input :: Monad m => Channel a -> Process m (Input a)
input (UnsafeChannel u) = Process $ \name -> do
  s <- State.get
  (u',us') <- go name $ Map.lookup u $ piInputs s
  State.put s { piInputs = Map.insert u us' $ piInputs s }
  return (UnsafeInput u')
 where
  go _ Nothing = return (u, [u])
  go name (Just us) = do
    u' <- U.Channel <$> sfresh name "dup"
    return (u', us <> [u'])


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


