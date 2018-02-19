{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternGuards #-}
module Bench.Plumbing.ParseInt (
    atoi
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as BI

import           P

import GHC.Prim
import GHC.Exts

{-# INLINE atoi #-}
atoi :: ByteString -> Maybe (Int, ByteString)
atoi as
 = let (# i,len #) = atoiUnsafe as
   in if I# len > 0
      then Just (I# i, B.drop (I# len) as)
      else Nothing

atoiUnsafe :: ByteString -> (# Int#, Int# #)
atoiUnsafe !as
 | B.null as = (# 0#, 0# #)
 | otherwise =
     case BI.w2c $ B.unsafeHead as of
         '-' -> getfst True  1#
         '+' -> getfst False 1#
         _   -> getfst False 0#

 where
  !(I# len) = B.length as

  getfst neg ix
   | I# ix < I# len
   , w <- B.unsafeIndex as (I# ix)
   , w >= 0x30
   , w <= 0x39
   = loop neg ix 0#
   | otherwise
   = (# 0#, 0# #)

  loop :: Bool -> Int# -> Int# -> (# Int#, Int# #)
  loop neg !ix !n
   | I# ix >= I# len
   = (# end neg n, ix #)
   | otherwise
   = case B.unsafeIndex as (I# ix) of
      w | w >= 0x30
       && w <= 0x39 ->
          let !(I# w') = fromIntegral w - 0x30
          in  loop neg (ix +# 1#) ((n *# 10#) +# w')
        | otherwise -> (# end neg n, ix #)

  end True n = negateInt# n
  end _    n = n

