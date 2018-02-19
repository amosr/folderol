{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Bench.Plumbing.Chunks (pullLine) where

import qualified System.IO as IO

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as Internal
import qualified Data.ByteString.Char8 as Char8

import GHC.IO
import GHC.Prim
import GHC.Exts
import GHC.ForeignPtr

-- Relatively low allocation chunked inputs.
-- We might reduce the allocation further by inlining everything, but this causes the code to blow up a bit
-- because "pullLine" returns "Just" in two cases, and GHC duplicates the consumer into both "Just" cases.
-- We would like to mark the consumer as a continuation, but I don't know how to achieve this.
-- But, if we're crazy, we can get the allocation-reduction benefits of inlining without the inlining itself.
-- This is really just a manual worker/wrapper transform where we get to choose the exact representation.

-- Split: The result of splitting a ByteString in two.
-- This is equivalent to an (Int, ByteString) which has been flattened and unboxed.
type Split = (# Int#, Addr#, ForeignPtrContents, Int#, Int# #)

{-# INLINE pullLine #-}
pullLine :: Char8.ByteString -> IO.Handle -> IO (Maybe Char8.ByteString, Char8.ByteString)
pullLine buf h = IO $ \rw0 ->
  let (# !rw1, !r #) = takeLineOrPull buf h rw0
  in  (# rw1, reify r #)

takeLineOrPull :: Char8.ByteString -> IO.Handle -> State# RealWorld -> (# State# RealWorld, Split #)
takeLineOrPull buf h rw0
 = let (# i, bp, bpc, bo, bl #) = takeLineUnsafe buf
   in  if I# i == -1
       then pullChunk buf h rw0
       else (# rw0, (# i, bp, bpc, bo, bl #) #)

pullChunk :: Char8.ByteString -> IO.Handle -> State# RealWorld -> (# State# RealWorld, Split #)
pullChunk leftover@(Internal.PS (ForeignPtr bp bpc) !(I# bo) !(I# bl)) h rw0 =
  let (# rw1, eof #)  = unIO (IO.hIsEOF h) rw0
  in case eof of
      True
       | Char8.null leftover ->
         (# rw1, (# -1#, bp, bpc, bo, bl #) #)
       | otherwise ->
         (# rw1, (# bl, bp, bpc, bo, bl #) #)
      False ->
       let (# rw2, buf0 #) = unIO (Char8.hGetSome h 4096) rw1
           buf1 = leftover `mappend` buf0
       in case takeLineUnsafe buf1 of
           (# i', bp', bpc', bo', bl' #)
            -- Should be very unlikely - only if line is longer than 4k
            | I# i' == -1
            -> pullChunk (Internal.PS (ForeignPtr bp' bpc') (I# bo') (I# bl')) h rw2
            | otherwise
            -> (# rw2, (# i', bp', bpc', bo', bl' #) #)

{-# INLINE takeLineUnsafe #-}
takeLineUnsafe :: Char8.ByteString -> Split
takeLineUnsafe b@(Internal.PS (ForeignPtr bp bpc) !(I# bo) !(I# bl)) =
 case Char8.elemIndex '\n' b of
  Nothing -> (# -1#, bp, bpc, bo, bl #)
  Just !(I# ix) ->
   let ix' = ix +# 1#
   in (# ix', bp, bpc, bo, bl #)

{-# INLINE reify #-}
reify :: Split -> (Maybe ByteString.ByteString, ByteString.ByteString)
reify (# i, bp, bpc, bo, bl #)
 | I# i == -1
 = (Nothing, Internal.PS (ForeignPtr bp bpc) (I# bo) (I# bl))
 | otherwise
 = (Just (Internal.PS (ForeignPtr bp bpc) (I# bo) (I# i)), Internal.PS (ForeignPtr bp bpc) (I# (bo +# i)) (I# (bl -# i)))

