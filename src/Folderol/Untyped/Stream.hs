{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Folderol.Untyped.Stream where

import Folderol.Untyped.Name
import qualified Folderol.Pretty as Pretty

import P

import qualified Language.Haskell.TH as Haskell

data Source
 = Source
 { sourcePull   :: Exp -- m (Maybe a)
 -- , sourceClose  :: Exp -- m ()
 }

data Sink
 = Sink
 { sinkPush     :: Exp -- a -> m ()
 -- , sinkClose    :: Exp -- m ()
 }

joinSinks :: Sink -> Sink -> Haskell.Q Sink
joinSinks (Sink a) (Sink b)
 = Sink . Exp <$> [| \i -> $(return $ unExp $ a) i >> $(return $ unExp $ b) i |]


instance Pretty.Pretty Source where
 pretty = Pretty.pretty . sourcePull

instance Pretty.Pretty Sink where
 pretty = Pretty.pretty . sinkPush

