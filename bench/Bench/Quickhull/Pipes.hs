{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
module Bench.Quickhull.Pipes where

import Bench.Quickhull.Skeleton
import Bench.Plumbing.Pipes
import qualified Pipes         as P
import qualified Pipes.Prelude as P
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox
import Data.IORef

import System.IO.Unsafe
import Control.Monad.Trans.Class (lift)

-- | Find left-most and right-most pivot points to start algorithm
-- Assert |xs| >= 1
{-# INLINE pivots #-}
pivots :: Unbox.Vector Point -> Line
pivots xs
 -- This would be easy enough to rewrite as a single fold
 = let l = Unbox.foldl1 (\(i,j) (x,y) -> if i < x then (i,j) else (x,y)) xs
       r = Unbox.foldl1 (\(i,j) (x,y) -> if i > x then (i,j) else (x,y)) xs
   in (l,r)


{-# INLINE filterMax #-}
-- Ideally this would be a pipeline something like:
--
-- vecR <- MUnbox.unsafeNew ...
-- runEffect (sourceVector ps
--        >-> map (\p -> (p, distance p l))
--        >-> fold (\(p1,d1) (p2,d2) -> if d1 > d2 then (p1,d1) else (p2,d2)) ((0,0),-1/0) fst
--        >-> filter (\(p,d) -> d > 0)
--        >-> map fst
--        >-> sinkVector vecR)
--
-- where the fold passes the values through unchanged, but returns the fold value at the end
filterMax :: Line -> Unbox.Vector Point -> (Point, Unbox.Vector Point)
filterMax l ps = unsafeDupablePerformIO $ do
  r  <- MUnbox.unsafeNew (Unbox.length ps)
  ix <- newIORef 0
  pt <- newIORef (0,0)
  P.runEffect (sourceVector ps        P.>->
               annot                  P.>->
               filterAndMax r ix pt 0 (0,0) (-1/0))
  pt' <- readIORef pt
  ix' <- readIORef ix
  r'  <- Unbox.unsafeFreeze $ MUnbox.unsafeSlice 0 ix' r
  return (pt', r')
 where
  annot = P.map (\p -> (p, distance p l))

  filterAndMax !vecR !ixR !ptR !ix (!x,!y) !d1 = do
    -- Since we cannot observe the end of the stream, we need to write an IORef every iteration
    -- in case this is our last.
    lift $ writeIORef ixR ix
    lift $ writeIORef ptR (x,y)
    (p2,d2) <- P.await
    let (!p',!d') = if d1 > d2 then ((x,y),d1) else (p2,d2)
    case d2 > 0 of
     True -> do
      lift $ MUnbox.unsafeWrite vecR ix p2
      filterAndMax vecR ixR ptR (ix+1) p' d'
     False -> do
      filterAndMax vecR ixR ptR ix p' d'


runQuickhull :: Unbox.Vector Int -> IO (Unbox.Vector Point)
runQuickhull is = do
  let ps = genPoints is
  let hull = quickhullWithPivots pivots filterMax ps
  hull `seq` return hull

