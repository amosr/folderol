{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Folderol.Untyped.Transform.Fusion where

import Folderol.Untyped.Name
import Folderol.Untyped.Network
import Folderol.Untyped.Process

import qualified Folderol.Internal.Haskell as Haskell

import           X.Control.Monad.Trans.Either (EitherT)
import qualified X.Control.Monad.Trans.Either as EitherT
import           Control.Monad.Trans.Class (MonadTrans(..))

import P

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

data InputState = IS'Have | IS'Closed
 deriving (Eq, Ord, Show)

data FuseLabel
 = FuseLabel
 { lblP :: Label
 , lblQ :: Label
 , isPQ :: Map Channel InputState
 }
 deriving (Eq, Ord, Show)

fuseNetwork :: NetworkGraph m -> Haskell.Q (NetworkGraph m)
fuseNetwork graph
 = do ps <- go (nProcesses graph)
      return graph { nProcesses = ps }
 where
  go [] = return []
  go [p] = return [p]
  go (p:ps)
   = case findConnected p [] ps of
      Nothing -> do
        ps' <- go ps
        return (p : ps')
      Just (q,ps') -> do
        f <- EitherT.runEitherT $ fusePair p q
        case f of
         Left err -> fail (show err)
         Right p' -> go (p' : ps')

  findConnected _ _ [] = Nothing
  findConnected p acc (q:ps)
   | connected p q
   = Just (q,acc <> ps)
   | otherwise
   = findConnected p (q : acc) ps

  connected p q
   = not
   $ Set.null
   $ Set.intersection
     (pInputs  p `Set.union` pInputs  q)
     (pOutputs p `Set.union` pOutputs q)

data Error
 -- | Can't fuse two processes because they appear to be inherently unbounded:
 -- In which case we want to help the user or introduce buffering or communication
 = Error'Fusion FusionError
 -- | Something went really wrong, like it's not a valid process or something
 | Error'Internal    InternalError
 deriving Show

data FusionError
 = CantFuse FuseLabel
 deriving Show

data InternalError
 = ProcessesShareInputs  (Set Channel) 
 | ProcessesShareOutputs (Set Channel) 
 | NoSuchLabel Label
 deriving Show


data ChannelType2
 = In1 | Out1 | In1Out1
 deriving (Eq, Ord, Show)

channels :: Process -> Process -> Either InternalError (Map Channel ChannelType2)
channels p q
 = let pis     = pInputs  p
       qis     = pInputs  q
       pos     = pOutputs p
       qos     = pOutputs q
       is      = pis `Set.union` qis
       os      = pos `Set.union` qos
       sis     = pis `Set.intersection` qis
       sos     = pis `Set.intersection` qis

       err s e = when (not $ Set.null s) $ Left e
       chn c   = Map.fromSet (const c)

       chans   = Map.unions
        [ chn In1     (is `Set.difference` os)
        , chn Out1    (os `Set.difference` is)
        , chn In1Out1 (is `Set.intersection` os) 
        ]
   in do  err sis $ ProcessesShareInputs sis
          err sos $ ProcessesShareOutputs sos
          return  $ chans

fusePair :: Process -> Process -> EitherT Error Haskell.Q Process
fusePair p q
 = do chans <- EitherT.firstEitherT Error'Internal $ EitherT.hoistEither $ channels p q

      let mkVar (Channel c) _ = Var <$> lift (Haskell.newName $ show c)
      vars <- sequence $ Map.mapWithKey mkVar $ Map.filter (==In1Out1) chans

      let initFL = FuseLabel (nextLabel $ pInitial p) (nextLabel $ pInitial q) Map.empty
      (fml,l0) <- insertFuseLabel Map.empty initFL
      instrs <- fixp (go vars) fml Map.empty [initFL]

      let init = Next l0 (Map.union (nextUpdates $ pInitial p) (nextUpdates $ pInitial q))

      let name    = pName p <> " / " <> pName q
      let inputs  = Map.keysSet $ Map.filter (==In1) chans
      let outputs = Map.keysSet $ Map.filter (/=In1) chans
      return Process
       { pName         = name
       , pInputs       = inputs
       , pOutputs      = outputs
       , pInitial      = init
       , pInstructions = instrs }
 where
  fixp _ _ instrs0 []
   = return instrs0
  fixp f fml0 instrs0 (fl:fls)
   | Just l <- Map.lookup fl fml0
   , Just _ <- Map.lookup l  instrs0
   = fixp f fml0 instrs0 fls
   | otherwise
   = do (fml1,l) <- insertFuseLabel fml0 fl
        (fml2,info,outs) <- f fml1 fl
        fixp f fml2 (Map.insert l info instrs0) (fls <> outs)

  go vars fml0 fl
   = do pinfo <- lookupInfo (lblP fl) p
        qinfo <- lookupInfo (lblQ fl) q
        (fml1,instr, outs) <- tryStepPair vars fml0 fl pinfo qinfo
        return (fml1, infoOf pinfo qinfo vars fl instr, outs)

tryStepPair :: Map Channel Var -> Map FuseLabel Label -> FuseLabel -> Info -> Info -> EitherT Error Haskell.Q (Map FuseLabel Label, Instruction, [FuseLabel])
tryStepPair vars fml0 fl (Info pbinds pinstr) (Info qbinds qinstr)
 | I'Done <- pinstr
 , I'Done <- qinstr
 = return (fml0, I'Done, [])
 | otherwise
 = do (fml1,pinstr') <- tryStep id      vars fml0 fl pinstr (updatesOfBinds qbinds)
      (fml2,qinstr') <- tryStep swapper vars fml1 (swapper fl) qinstr (updatesOfBinds pbinds)
      case (pinstr', qinstr') of
       (Nothing, Nothing) -> EitherT.left $ Error'Fusion (CantFuse fl)
       (Just (i1,outs1), Just (i2,outs2))
        | (i,outs) <- preference i1 outs1 i2 outs2
        -> return (fml2, i, outs)
       (Just (i,outs), _) -> return (fml2, i, outs)
       (_, Just (i,outs)) -> return (fml2, i, outs)
 where
  preference i1 outs1 i2 outs2
   | I'Done <- pinstr
   = (i2, outs2)
   | I'Done <- qinstr
   = (i1, outs1)
   | I'Jump{} <- i1
   = (i1, outs1)
   | I'Jump{} <- i2
   = (i2, outs2)
   | I'Pull{} <- i2
   = (i1, outs1)
   | I'Pull{} <- i1
   = (i2, outs2)
   | otherwise
   = (i1, outs1)

  swapper (FuseLabel flp flq fis) = FuseLabel flq flp fis

  updatesOfBinds
   = Map.fromSet (Haskell.VarE . unVar)

tryStep :: (FuseLabel -> FuseLabel) -> Map Channel Var -> Map FuseLabel Label -> FuseLabel -> Instruction -> Map Var Haskell.Exp -> EitherT Error Haskell.Q (Map FuseLabel Label, Maybe (Instruction, [FuseLabel]))
tryStep swapper vars fml0 fl instruction otherUpdates
 -- TODO: compute instruction
 = case instruction of
   I'Done
    -- TODO: these closes should be separate instructions
    -> do (fml1,fl',n') <- mkNext fml0 (lblP fl) (fmap (const IS'Closed) vars) Map.empty
          return (fml1, Just (I'Jump n', [fl']))

   I'Jump n
    -> do (fml1,fl',n') <- mkNext fml0 (nextLabel n) (isPQ fl) (nextUpdates n)
          return (fml1, Just (I'Jump n', [fl']))
   I'Bool e n1 n2
    -> do (fml1,fl1',n1') <- mkNext fml0 (nextLabel n1) (isPQ fl) (nextUpdates n1)
          (fml2,fl2',n2') <- mkNext fml1 (nextLabel n2) (isPQ fl) (nextUpdates n2)
          return (fml2, Just (I'Bool e n1' n2', [fl1', fl2']))

   I'Pull c v n1 n2
    | Just buf <- Map.lookup c vars
    , is <- Map.lookup c $ isPQ fl
    -> case is of
        Nothing -> return (fml0, Nothing)

        Just IS'Have -> do
          let m = Map.insert v (Haskell.VarE $ unVar buf)
                $ nextUpdates n1
          (fml1,fl1',n1') <- mkNext fml0 (nextLabel n1) (isPQ fl) m
          return (fml1, Just (I'Jump n1', [fl1']))

        Just IS'Closed -> do
          (fml1,fl2',n2') <- mkNext fml0 (nextLabel n2) (isPQ fl) (nextUpdates n2)
          return (fml1, Just (I'Jump n2', [fl2']))

    | otherwise
    -> do (fml1,fl1',n1') <- mkNext fml0 (nextLabel n1) (isPQ fl) (nextUpdates n1)
          (fml2,fl2',n2') <- mkNext fml1 (nextLabel n2) (isPQ fl) (nextUpdates n2)
          return (fml2, Just (I'Pull c v n1' n2', [fl1', fl2']))

   I'Push c e n
    | Just buf <- Map.lookup c vars
    , is <- Map.lookup c $ isPQ fl
    -> case is of
        Nothing -> do
          -- TODO: don't duplicate expression
          let m = Map.insert buf e
                $ nextUpdates n
          (fml1,fl',n') <- mkNext fml0 (nextLabel n) (Map.insert c IS'Have $ isPQ fl) m
          return (fml1, Just (I'Push c e n', [fl']))

        Just IS'Have -> return (fml0, Nothing)

        -- Other machine is Done?...
        Just IS'Closed -> do
          (fml1,fl',n') <- mkNext fml0 (nextLabel n) (isPQ fl) (nextUpdates n)
          return (fml1, Just (I'Push c e n', [fl']))

    | otherwise
    -> do (fml1,fl',n') <- mkNext fml0 (nextLabel n) (isPQ fl) (nextUpdates n)
          return (fml1, Just (I'Push c e n', [fl']))


   I'Drop c n
    | Just _ <- Map.lookup c vars
    -> do (fml1,fl',n') <- mkNext fml0 (nextLabel n) (Map.delete c $ isPQ fl) (nextUpdates n)
          return (fml1, Just (I'Jump n', [fl']))

    | otherwise
    -> do (fml1,fl',n') <- mkNext fml0 (nextLabel n) (isPQ fl) (nextUpdates n)
          return (fml1, Just (I'Drop c n', [fl']))

 where
  mkNext fml l is updates
   = do let fl' = swapper $ fl { lblP = l, isPQ = is }
        (fml', l') <- insertFuseLabel fml fl'
        return (fml', fl', Next l' $ Map.unions [updates, otherUpdates, updatesOfFuseLabel vars fl'])

infoOf :: Info -> Info -> Map Channel Var -> FuseLabel -> Instruction -> Info
infoOf pinfo qinfo vars fl instruction
 = let bs = Set.unions [ infoBindings pinfo, infoBindings qinfo, varsOfFuseLabel vars fl ]
   in  Info bs instruction

lookupInfo :: Label -> Process -> EitherT Error Haskell.Q Info
lookupInfo l p
 | Just i <- Map.lookup l $ pInstructions p
 = return i
 | otherwise
 = EitherT.left $ Error'Internal $ NoSuchLabel l

varsOfFuseLabel :: Map Channel Var -> FuseLabel -> Set Var
varsOfFuseLabel vars fl
 = Set.fromList
 $ Map.elems
 $ Map.intersection vars
 $ Map.filter (\c -> c == IS'Have)
 $ isPQ fl

updatesOfFuseLabel :: Map Channel Var -> FuseLabel -> Map Var Haskell.Exp
updatesOfFuseLabel vars fl
 = Map.fromSet (Haskell.VarE . unVar)
 $ varsOfFuseLabel vars fl


insertFuseLabel :: Map FuseLabel Label -> FuseLabel -> EitherT Error Haskell.Q (Map FuseLabel Label, Label)
insertFuseLabel fml fl
 | Just l <- Map.lookup fl fml
 = return (fml, l)
 | otherwise
 = do l' <- Label <$> lift (Haskell.newName "fuse")
      return (Map.insert fl l' fml, l')

