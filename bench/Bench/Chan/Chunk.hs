{-# LANGUAGE BangPatterns #-}
module Bench.Chan.Chunk where

import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

-- The chunking has to copy from the input vector into a new vector.
-- We could really just slice out from the original vector in this case,
-- but that would be an unfair benchmark because the real case won't have a manifest vector.
--
{-# INLINE pushChunk #-}
pushChunk :: Int -> Vector.Vector v -> (Vector.Vector v -> IO ()) -> IO ()
pushChunk chunkSize !input push = do
  v0 <- MVector.unsafeNew chunkSize
  go 0 v0 0
 where
  go i mv j
   | i == Vector.length input
   = do v <- Vector.unsafeFreeze $ MVector.unsafeSlice 0 j mv
        -- Push the leftovers then an empty vector to signal the end.
        -- We could optimise this case slightly by only pushing an empty if j < chunkSize,
        -- but since this only happens at the end I don't think it's worth it.
        push v
        push Vector.empty
   | j == chunkSize
   = do -- Either way we need to allocate a new array.
        -- We could copy the vector and send it, reusing the mutable buffer,
        -- or freeze the mutable buffer and send a new one.
        -- I suspect allocating a new uninitialised one is faster than copying.
        v <- Vector.unsafeFreeze mv
        push v
        mv' <- MVector.unsafeNew chunkSize
        go i mv' 0
   | otherwise
   -- j < chunkSize
   = do MVector.unsafeWrite mv j (Vector.unsafeIndex input i)
        go (i + 1) mv (j + 1)

{-# INLINE pullChunk #-}
pullChunk :: (a -> IO ()) -> (IO (Vector.Vector a)) -> IO ()
pullChunk each pull = go
 where
  go = do
    v <- pull
    case Vector.null v of
      True -> return ()
      False -> mapM_ each v >> go

