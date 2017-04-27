{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Array.Folderol where

import Bench.Plumbing
import Bench.Array.Helper

import Folderol
import Folderol.Splice

import qualified Folderol.Source as Source

import qualified Data.Vector.Unboxed as Unbox

import Prelude hiding (filter, map)


runFilter :: Unbox.Vector Int -> IO (Unbox.Vector Int)
runFilter vec = do
 (above,()) <- vectorAtMostIO (Unbox.length vec) $ \snkAbove -> do
  $$(fuse defaultFuseOptions $ do
     ins   <- source [|| Source.sourceOfVector vec ||]
     above <- filter [||(>0)||] ins
     sink above [|| snkAbove ||])

 return above

runFilterFlip :: Unbox.Vector Int -> IO (Unbox.Vector Int)
runFilterFlip vec = do
 (above,()) <- vectorAtMostIO (Unbox.length vec) $ \snkAbove -> do
  $$(fuse defaultFuseOptions $ do
     ins   <- source [|| Source.sourceOfVectorFlip vec ||]
     above <- filter [||(>0)||] ins
     sink above [|| snkAbove ||])

 return above


runFilterGrow :: Unbox.Vector Int -> IO (Unbox.Vector Int)
runFilterGrow vec = do
 (above,()) <- vectorIO $ \snkAbove -> do
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

runFilterMaxGrow :: Unbox.Vector Int -> IO (Int, Unbox.Vector Int)
runFilterMaxGrow vec = do
 (maxim,(above,())) <- scalarIO $ \snkMaxim -> vectorIO $ \snkAbove -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]

      maxim <- fold   [||max||] [||0||] ins
      above <- filter [||(>0)||] ins

      sink maxim [|| snkMaxim ||]
      sink above [|| snkAbove ||])
 return (maxim, above)



runPartition :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runPartition vec = do
 (below,(above,())) <- vectorAtMostIO (Unbox.length vec) $ \snkBelow -> vectorAtMostIO (Unbox.length vec) $ \snkAbove -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]

      (below,above) <- partition [||(<=0)||] ins

      sink below [|| snkBelow ||]
      sink above [|| snkAbove ||])
 return (below, above)


runPartitionGrow :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runPartitionGrow vec = do
 (below,(above,())) <- vectorIO $ \snkBelow -> vectorIO $ \snkAbove -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]

      (below,above) <- partition [||(<=0)||] ins

      sink below [|| snkBelow ||]
      sink above [|| snkAbove ||])
 return (below, above)

runPartitionMap2 :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runPartitionMap2 vec = do
 (below,(above,())) <- vectorAtMostIO (Unbox.length vec) $ \snkBelow -> vectorAtMostIO (Unbox.length vec) $ \snkAbove -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]

      (below,above) <- partition [||(<=0)||] ins

      below' <- map [||expensive1||] below
      above' <- map [||expensive2||] above

      sink below' [|| snkBelow ||]
      sink above' [|| snkAbove ||])
 return (below, above)

runPartitionMap2Grow :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runPartitionMap2Grow vec = do
 (below,(above,())) <- vectorIO $ \snkBelow -> vectorIO $ \snkAbove -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]

      (below,above) <- partition [||(<=0)||] ins

      below' <- map [||expensive1||] below
      above' <- map [||expensive2||] above

      sink below' [|| snkBelow ||]
      sink above' [|| snkAbove ||])
 return (below, above)


runMapPartition :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runMapPartition vec = do
 (below,(above,())) <- vectorAtMostIO (Unbox.length vec) $ \snkBelow -> vectorAtMostIO (Unbox.length vec) $ \snkAbove -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]

      ins'  <- map [||expensive||] ins

      (below,above) <- partition [||(<=0)||] ins'

      sink below [|| snkBelow ||]
      sink above [|| snkAbove ||])
 return (below, above)

runMapPartitionGrow :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runMapPartitionGrow vec = do
 (below,(above,())) <- vectorIO $ \snkBelow -> vectorIO $ \snkAbove -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]

      ins'  <- map [||expensive||] ins

      (below,above) <- partition [||(<=0)||] ins'

      sink below [|| snkBelow ||]
      sink above [|| snkAbove ||])
 return (below, above)

runMapPartitionMap2 :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runMapPartitionMap2 vec = do
 (below,(above,())) <- vectorAtMostIO (Unbox.length vec) $ \snkBelow -> vectorAtMostIO (Unbox.length vec) $ \snkAbove -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]

      ins'          <- map [||expensive||] ins
      (below,above) <- partition [||(<=0)||] ins'
      below'        <- map [||expensive1||] below
      above'        <- map [||expensive2||] above

      sink below' [|| snkBelow ||]
      sink above' [|| snkAbove ||])
 return (below, above)

runMapPartitionMap2Grow :: Unbox.Vector Int -> IO (Unbox.Vector Int, Unbox.Vector Int)
runMapPartitionMap2Grow vec = do
 (below,(above,())) <- vectorIO $ \snkBelow -> vectorIO $ \snkAbove -> do
   $$(fuse defaultFuseOptions $ do
      ins <- source [|| Source.sourceOfVector vec ||]

      ins'          <- map [||expensive||] ins
      (below,above) <- partition [||(<=0)||] ins'
      below'        <- map [||expensive1||] below
      above'        <- map [||expensive2||] above

      sink below' [|| snkBelow ||]
      sink above' [|| snkAbove ||])
 return (below, above)


