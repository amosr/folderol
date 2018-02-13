{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Bench.Plumbing.Chunks (pullLine) where

import qualified System.IO as IO

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as Internal
import qualified Data.ByteString.Char8 as Char8
import Data.Word

import GHC.IO
import GHC.Prim
import GHC.Exts
import Foreign.ForeignPtr


type Res = (# Int#, ForeignPtr Word8, Int#, Int# #)

{-# INLINE pullLine #-}
pullLine :: Char8.ByteString -> IO.Handle -> IO (Maybe Char8.ByteString, Char8.ByteString)
pullLine buf h = IO $ \rw0 ->
  let (# !rw1, !r #) = takeLineOrPull buf h rw0
  in  (# rw1, reify r #)

takeLineOrPull :: Char8.ByteString -> IO.Handle -> State# RealWorld -> (# State# RealWorld, Res #)
takeLineOrPull buf h rw0
 = let (# i, bp, bo, bl #) = takeLineUnsafe buf
   in  if I# i == -1
       then pullChunk buf h rw0
       else (# rw0, (# i, bp, bo, bl #) #)

pullChunk :: Char8.ByteString -> IO.Handle -> State# RealWorld -> (# State# RealWorld, Res #)
pullChunk !leftover@(Internal.PS bp !(I# bo) !(I# bl)) h rw0 =
  let (# rw1, eof #)  = unIO (IO.hIsEOF h) rw0
  in case eof of
      True
       | Char8.null leftover ->
         (# rw1, (# -1#, bp, bo, bl #) #)
       | otherwise ->
         (# rw1, (# bl, bp, bo, bl #) #)
      False ->
       let (# rw2, buf0 #) = unIO (Char8.hGetSome h 4096) rw1
           buf1 = leftover `mappend` buf0
       in case takeLineUnsafe buf1 of
           (# i', bp', bo', bl' #)
            -- Should be very unlikely - only if line is longer than 4k
            | I# i' == -1
            -> pullChunk (Internal.PS bp' (I# bo') (I# bl')) h rw2
            | otherwise
            -> (# rw2, (# i', bp', bo', bl' #) #)

{-# INLINE takeLineUnsafe #-}
takeLineUnsafe :: Char8.ByteString -> Res
takeLineUnsafe b@(Internal.PS bp !(I# bo) !(I# bl)) =
 case Char8.elemIndex '\n' b of
  Nothing -> (# -1#, bp, bo, bl #)
  Just !(I# ix) ->
   let ix' = ix +# 1#
   in (# ix', bp, bo, bl #)

{-# INLINE reify #-}
reify :: Res -> (Maybe ByteString.ByteString, ByteString.ByteString)
reify (# i, bp, bo, bl #)
 | I# i == -1
 = (Nothing, Internal.PS bp (I# bo) (I# bl))
 | otherwise
 = (Just (Internal.PS bp (I# bo) (I# i)), Internal.PS bp (I# (bo +# i)) (I# (bl -# i)))

