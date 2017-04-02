{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Untyped.Codegen where

import Folderol.Untyped.Name
import Folderol.Untyped.Process
import Folderol.Untyped.Stream
import Folderol.Untyped.Network

import Folderol.Spawn (Spawn)
import qualified Folderol.Spawn as Spawn

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import P

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Folderol.Internal.Haskell as Haskell

-- | Generate code for a network - error if there is more than one process!
genNetwork1 :: Monad m => NetworkGraph m -> Haskell.TExpQ (m ())
genNetwork1 graph
 = case nProcesses graph of
    []    -> [||return ()||]
    [p]   -> genProcess (nSources graph) (nSinks graph) p
    (_:_) -> fail "Unable to generate code for network with multiple processes. Some processes were not fused together."


-- | Generate code for a network, inserting communication channels between processes
genNetwork :: Spawn m => NetworkGraph m -> Haskell.TExpQ (m ())
genNetwork graph0
 = go graph0 $ Set.toList $ interprocess $ nProcesses graph0
 where
  go g [] = genNetworkProcesses g
  go g (c:cs) = do
    c'src <- Haskell.newName ("src" <> show (unChannel c))
    c'snk <- Haskell.newName ("snk" <> show (unChannel c))

    e'src <- Source <$> (Haskell.unsafeTExpCoerce $ Haskell.varE c'src)
    e'snk <- Sink   <$> (Haskell.unsafeTExpCoerce $ Haskell.varE c'snk)

    g' <- joinNetworks g 
        $ NetworkGraph { nSources   = Map.singleton c e'src
                       , nSinks     = Map.singleton c e'snk
                       , nProcesses = [] }

    Haskell.unsafeTExpCoerce (
      (Haskell.varE '(>>=) `Haskell.appE` (Haskell.varE 'Spawn.channel)) `Haskell.appE`
      (Haskell.lamE [Haskell.conP '(,) [Haskell.varP c'snk, Haskell.varP c'src]]
                 $ (Haskell.unType <$> go g' cs)))


genNetworkProcesses :: Spawn m => NetworkGraph m -> Haskell.TExpQ (m ())
genNetworkProcesses graph
 = case nProcesses graph of
    []  -> [||return ()||]
    [p] -> genProcess (nSources graph) (nSinks graph) p
    (p:ps) ->
     [||Spawn.join2 $$(genProcess (nSources graph) (nSinks graph) p)
              $$(genNetworkProcesses (graph { nProcesses = ps}) )||]

interprocess :: [Process] -> Set Channel
interprocess ps0
 = go Set.empty [] ps0
 where
  go acc _ [] = acc
  go acc ps' (p:ps)
   = let ins  = Set.unions $ fmap pInputs  (ps' <> ps)
         this = Set.intersection ins (pOutputs p)
     in  go (Set.union acc this) (p : ps') ps



genProcess :: Map Channel (Source m) -> Map Channel (Sink m) -> Process -> Haskell.TExpQ (m ())
genProcess sources sinks proc
 = do let mapFilter set = Map.filterWithKey (\k _ -> Set.member k set)
      let sources' = mapFilter (pInputs  proc) sources
      let sinks'   = mapFilter (pOutputs proc) sinks

      body <- Haskell.letE
                 (fmap (genInstruction sources' sinks') $ Map.toList $ pInstructions proc)
                 (genNext sources' sinks' $ pInitial proc)

      let body'    = foldr (uncurry unwrapSource) body  (Map.toList sources')
      let body''   = foldr (uncurry unwrapSink)   body' (Map.toList sinks')
      Haskell.unsafeTExpCoerce $ return body''

unwrapSource :: Channel -> Source m -> Haskell.Exp -> Haskell.Exp
unwrapSource c (Source s) body
 = let init = chanName c "init"
       pull = chanName c "pull"
       done = chanName c "done"
       state = chanName c "state"
       lamb = Haskell.LamE [Haskell.VarP state] body
       inity = (Haskell.VarE '(>>=) `Haskell.AppE` Haskell.VarE init)  `Haskell.AppE` lamb
   in Haskell.CaseE (Haskell.unType s)
    [ Haskell.Match
    ( Haskell.ConP 'Source.Source [Haskell.VarP init, Haskell.VarP pull, Haskell.VarP done] )
    ( Haskell.NormalB inity )
      []
    ]

unwrapSink :: Channel -> Sink m -> Haskell.Exp -> Haskell.Exp
unwrapSink c (Sink s) body
 = let init = chanName c "init"
       push = chanName c "push"
       done = chanName c "done"
       state = stateName c
       lamb = Haskell.LamE [Haskell.VarP state] body
       inity = (Haskell.VarE '(>>=) `Haskell.AppE` Haskell.VarE init)  `Haskell.AppE` lamb
   in Haskell.CaseE (Haskell.unType s)
    [ Haskell.Match
    ( Haskell.ConP 'Sink.Sink [Haskell.VarP init, Haskell.VarP push, Haskell.VarP done] )
    ( Haskell.NormalB inity )
      []
    ]

chanName :: Channel -> [Char] -> Haskell.Name
chanName (Channel c) str = Haskell.mkName (show c <> "_" <> str)

stateName :: Channel -> Haskell.Name
stateName c = chanName c "state"

genNext :: Map Channel (Source m) -> Map Channel (Sink m) -> Next -> Haskell.ExpQ
genNext srcs snks (Next ll bs)
  = return $ foldl Haskell.AppE (Haskell.VarE $ unLabel ll) args
 where
  args = fmap st' (Map.keys srcs) <> fmap st' (Map.keys snks) <> fmap snd (Map.toList bs)
  st' = Haskell.VarE . stateName

genInstruction :: Map Channel (Source m) -> Map Channel (Sink m) -> (Label, Info) -> Haskell.DecQ
genInstruction sources sinks (l, info)
 = do body <- bodyQ $ infoInstruction info
      let clause = Haskell.Clause (bindsSt <> binds) (Haskell.NormalB body) []
      return $ Haskell.FunD l' [clause]
 where
  l'    = unLabel l

  bindsSt = fmap (Haskell.VarP . stateName) (Map.keys sources) <> fmap (Haskell.VarP . stateName) (Map.keys sinks)
  binds = fmap (Haskell.VarP . unVar)
        $ Set.toList
        $ infoBindings info

  genNext' = genNext sources sinks

  bodyQ = \case
    I'Done 
      -> let closeS b c = (Haskell.varE '(>>) `Haskell.appE` (Haskell.varE (chanName c "done") `Haskell.appE` Haskell.varE (stateName c))) `Haskell.appE` b
         in  foldl closeS [|return ()|] (Map.keys sources <> Map.keys sinks)
    I'Jump n
      -> genNext' n
    I'Bool b t f
      -> [| if $(return b) then $(genNext' t) else $(genNext' f) |]
    I'Drop _  n
      -> genNext' n
    I'Pull c v t f
      | Map.member c sources
      -> do i <- Haskell.newName "pulled"

            let s0    = Haskell.varE $ stateName c
            let pull0 = Haskell.varE $ chanName c "pull"
            let bnd  = Haskell.varE '(>>=)

            let body0 = Haskell.lamE [return $ Haskell.ConP '(,) [Haskell.VarP i, Haskell.VarP (stateName c)]]
                      [|case $(Haskell.varE i) of 
                         Just val -> $(genNextWith t v 'val)
                         Nothing -> $(genNext' f)|]

            bnd `Haskell.appE` (pull0 `Haskell.appE` s0) `Haskell.appE` body0

      | otherwise
      -> genNext' f
    I'Push c e n
      | Map.member c sinks
      -> do let s0    = Haskell.varE $ stateName c
            let push0 = Haskell.varE $ chanName c "push"
            let bnd  = Haskell.varE '(>>=)

            let body0 = Haskell.lamE [return $ Haskell.VarP (stateName c)] [|$(genNext' n)|]

            bnd `Haskell.appE` ((push0 `Haskell.appE` s0) `Haskell.appE` return e) `Haskell.appE` body0

      | otherwise
      -> genNext' n

  genNextWith (Next ll uu) xx ee
   = genNext' $ Next ll (Map.insert xx (Haskell.VarE ee) uu)

