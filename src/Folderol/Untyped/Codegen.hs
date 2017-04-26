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

import GHC.Types (SPEC(..))

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
    -- Insert read and write channels
    c'src <- Haskell.newName ("src" <> show (unChannel c))
    c'snk <- Haskell.newName ("snk" <> show (unChannel c))

    e'src <- Source <$> (Haskell.unsafeTExpCoerce $ Haskell.varE c'src)
    e'snk <- Sink   <$> (Haskell.unsafeTExpCoerce $ Haskell.varE c'snk)

    -- It is a bit sneaky to have a source and a sink for same channel: each process must only uses sinks for outputs, and only uses sources for inputs
    g' <- joinNetworks g 
        $ createNetwork (Map.singleton c e'src) (Map.singleton c e'snk) []

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

-- | Find the set of channels that span across processes
interprocess :: [Process] -> Set Channel
interprocess ps0
 = go Set.empty [] ps0
 where
  go acc _ [] = acc
  go acc ps' (p:ps)
   = let ins  = Set.unions $ fmap pInputs  (ps' <> ps)
         this = Set.intersection ins (pOutputs p)
     in  go (Set.union acc this) (p : ps') ps


-- | Generate code for a single process
genProcess :: Map Channel (Source m) -> Map Channel (Sink m) -> Process -> Haskell.TExpQ (m ())
genProcess sources sinks proc
 = do let mapFilter set = Map.filterWithKey (\k _ -> Set.member k set)
      -- Make sure we only get the relevant sources and sinks.
      -- If process has input C, source for C is used, but sink for C is not.
      -- This is important for inserting concurrent channel communication (see above).
      let sources' = mapFilter (pInputs  proc) sources
      let sinks'   = mapFilter (pOutputs proc) sinks

      -- Generate the call to initial label, and initialise all the sources and sinks around it.
      -- The initialisation binds (stateName c), so it must not shadow any label definitions.
      body0    <- genNext (fmap (const False) sources') (fmap (const False) sinks') $ pInitial proc
      let body1 = foldr unwrapInit body0 (Map.keys sources' <> Map.keys sinks')

      instrs <- mapM (genInstruction sources' sinks') $ Map.toList $ pInstructions proc
      -- Generate all the label definitions.
      let body2 = Haskell.LetE (concat instrs) body1

      -- Unwrap the sources and sinks.
      -- These bind functions like c_init, c_pull or c_push, and c_done.
      -- This has to be done at the top level because the states are existentially quantified.
      let body3 = foldr (uncurry unwrapSource) body2 (Map.toList sources')
      let body4 = foldr (uncurry unwrapSink)   body3 (Map.toList sinks')

      Haskell.unsafeTExpCoerce $ return body4

unwrapSource :: Channel -> Source m -> Haskell.Exp -> Haskell.Exp
unwrapSource c (Source s) body
 = let init = chanName c "init"
       pull = chanName c "pull"
       done = chanName c "done"
   in Haskell.CaseE (Haskell.unType s)
    [ Haskell.Match
    ( Haskell.ConP 'Source.Source [Haskell.VarP init, Haskell.VarP pull, Haskell.VarP done] )
    ( Haskell.NormalB body )
      []
    ]

unwrapSink :: Channel -> Sink m -> Haskell.Exp -> Haskell.Exp
unwrapSink c (Sink s) body
 = let init = chanName c "init"
       push = chanName c "push"
       done = chanName c "done"
   in Haskell.CaseE (Haskell.unType s)
    [ Haskell.Match
    ( Haskell.ConP 'Sink.Sink [Haskell.VarP init, Haskell.VarP push, Haskell.VarP done] )
    ( Haskell.NormalB body )
      []
    ]

unwrapInit :: Channel -> Haskell.Exp -> Haskell.Exp
unwrapInit c body
 = let init = chanName c "init"
       state = chanName c "state"
       lamb = Haskell.LamE [Haskell.VarP state] body
       inity = (Haskell.VarE '(>>=) `Haskell.AppE` Haskell.VarE init)  `Haskell.AppE` lamb
   in inity


chanName :: Channel -> [Char] -> Haskell.Name
chanName (Channel c) str = Haskell.mkName (show c <> "_" <> str)

-- | State binding name, boolean means whether it's the updated binding
stateName' :: Channel -> Bool -> Haskell.Name
stateName' c True = chanName c "state'"
stateName' c False = chanName c "state"

stateName :: Channel -> Haskell.Name
stateName c = stateName' c False


genNext :: Map Channel Bool -> Map Channel Bool -> Next -> Haskell.ExpQ
genNext srcs snks (Next ll bs)
  = return $ foldl Haskell.AppE (Haskell.VarE $ unLabel ll) (Haskell.ConE 'SPEC : args)
 where
  args = fmap st' (Map.toList srcs) <> fmap st' (Map.toList snks) <> fmap snd (Map.toList bs)
  st' (c,b) = Haskell.VarE $ stateName' c b

genInstruction :: Map Channel (Source m) -> Map Channel (Sink m) -> (Label, Info) -> Haskell.Q [Haskell.Dec]
genInstruction sources sinks (l, info)
 = do body <- bodyQ $ infoInstruction info

      -- Insert wildcard case expressions for every binding to silence -Wunused-binds
      forced <- foldM insertForce body $ Set.toList $ infoBindings info 
      let clause = Haskell.Clause (bindsSpec <> bindsSt <> binds) (Haskell.NormalB forced) []
      let inline = Haskell.InlineP l' Haskell.Inline Haskell.ConLike Haskell.AllPhases
      return [Haskell.PragmaD inline, Haskell.FunD l' [clause] ]
 where
  l'    = unLabel l

  bindsSpec = [Haskell.ConP 'SPEC []]
  bindsSt = fmap (Haskell.VarP . stateName) (Map.keys sources) <> fmap (Haskell.VarP . stateName) (Map.keys sinks)
  binds = fmap (Haskell.VarP . unVar)
        $ Set.toList
        $ infoBindings info

  insertForce b v
   = [|case $(Haskell.varE $ unVar v) of
          _ -> $(return b)
     |]

  sourcesF = fmap (const False) sources
  sinksF   = fmap (const False) sinks
  genNext' = genNext sourcesF sinksF

  genNextUpdateState c
   = genNext (Map.adjust (const True) c sourcesF)
             (Map.adjust (const True) c sinksF)

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

            let body0 = Haskell.lamE [return $ Haskell.ConP '(,) [Haskell.VarP i, Haskell.VarP (stateName' c True)]]
                      [|case $(Haskell.varE i) of 
                         Just val -> $(genNextWith c t v 'val)
                         Nothing -> $(genNextUpdateState c f)|]

            bnd `Haskell.appE` (pull0 `Haskell.appE` s0) `Haskell.appE` body0

      | otherwise
      -> genNext' f
    I'Push c e n
      | Map.member c sinks
      -> do let s0    = Haskell.varE $ stateName c
            let push0 = Haskell.varE $ chanName c "push"
            let bnd  = Haskell.varE '(>>=)

            let body0 = Haskell.lamE [return $ Haskell.VarP (stateName' c True)] [|$(genNextUpdateState c n)|]

            bnd `Haskell.appE` ((push0 `Haskell.appE` s0) `Haskell.appE` return e) `Haskell.appE` body0

      | otherwise
      -> genNext' n

  genNextWith c (Next ll uu) xx ee
   = genNextUpdateState c $ Next ll (Map.insert xx (Haskell.VarE ee) uu)

