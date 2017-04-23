{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Array.Folderol where

import Folderol
import Folderol.Splice

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import qualified Data.Vector.Mutable as MVector
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox

import Prelude hiding (filter)
import Control.Monad.Primitive


runFilter :: Unbox.Vector Int -> IO (Unbox.Vector Int)
runFilter vec = do
 (above,()) <- vectorAtMostIO (Unbox.length vec) $ \snkAbove -> do
  $$(fuse defaultFuseOptions $ do
     ins   <- source [|| Source.sourceOfVector vec ||]
     above <- filter [||(>0)||] ins
     sink above [|| snkAbove ||])

 return above

runMax :: Unbox.Vector Int -> IO Int
runMax vec = do
 (maxim,()) <- scalarIO $ \snkMaxim -> do
  $$(fuse defaultFuseOptions $ do

     ins <- source [|| Source.sourceOfVector vec ||]
     maxim <- fold [||max||] [||0||] ins

     sink maxim [|| snkMaxim ||])

 return maxim


runFilterMax :: Unbox.Vector Int -> IO (Int, Unbox.Vector Int)
runFilterMax vec = do
 (maxim,(above,())) <- scalarIO $ \snkMaxim -> vectorAtMostIO (Unbox.length vec) $ \snkAbove -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]

      maxim <- fold   [||max||] [||0||] ins
      above <- filter [||(>0)||] ins

      sink maxim [|| snkMaxim ||]
      sink above [|| snkAbove ||])
 return (maxim, above)


{-# INLINE scalarIO #-}
scalarIO :: (Unbox.Unbox a) => (Sink.Sink IO a -> IO b) -> IO (a,b)
scalarIO = scalar

{-# INLINE vectorIO #-}
vectorIO :: Unbox.Unbox a => (Sink.Sink IO a -> IO b) -> IO (Unbox.Vector a, b)
vectorIO = vector

{-# INLINE vectorAtMostIO #-}
vectorAtMostIO :: Unbox.Unbox a => Int -> (Sink.Sink IO a -> IO b) -> IO (Unbox.Vector a, b)
vectorAtMostIO = vectorAtMost

{-# INLINE scalar #-}
scalar :: forall m a b
        . (PrimMonad m, Unbox.Unbox a)
       => (Sink.Sink m a -> m b) -> m (a,b)
scalar f = do
 ref <- MUnbox.unsafeNew 1 :: m (MUnbox.MVector (PrimState m) a)
 b   <- f $ Sink.scalarOfChannel ref
 a   <- MUnbox.unsafeRead ref 0
 return (a, b)

{-# INLINE vector #-}
vector :: forall m a b
        . (PrimMonad m, Unbox.Unbox a)
       => (Sink.Sink m a -> m b) -> m (Unbox.Vector a, b)
vector f = do
 ref <- MVector.unsafeNew 1 -- :: m (MVector.MVector (PrimState m) a)
 b   <- f $ Sink.vectorOfChannel'Generic ref
 a   <- MVector.unsafeRead ref 0
 return (a, b)

{-# INLINE vectorAtMost #-}
vectorAtMost :: forall m a b
        . (PrimMonad m, Unbox.Unbox a)
       => Int -> (Sink.Sink m a -> m b) -> m (Unbox.Vector a, b)
vectorAtMost len f = do
 ref <- MVector.unsafeNew 1 -- :: m (MVector.MVector (PrimState m) a)
 b   <- f $ Sink.vectorOfChannelAtMost len ref
 a   <- MVector.unsafeRead ref 0
 return (a, b)

