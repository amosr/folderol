{-# LANGUAGE BangPatterns #-}
module Bench.Plumbing.Chunks where

import qualified System.IO as IO

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

-- Not the default, because I can't be bothered implementing this for all the different libraries (Conduit, Streaming etc).
-- But maybe I should implement it.
{-# INLINE pullLine #-}
pullLine :: Char8.ByteString -> IO.Handle -> IO (Maybe Char8.ByteString, Char8.ByteString)
pullLine buf h = case takeLine buf of
  Just (line,rest) ->
    return (Just line, rest)
  Nothing ->
    puller buf
 where
  puller leftover = do
    e <- IO.hIsEOF h
    case e of
     True
      | Char8.null leftover ->
        return (Nothing, leftover)
      | otherwise ->
        return (Just leftover, Char8.empty)
     False -> do
      buf' <- Char8.hGetSome h 4096
      case takeLine buf' of
        Just (line,rest) -> return (Just (leftover `mappend` line), rest)
        Nothing -> puller (leftover `mappend` buf')

{-# INLINE takeLine #-}
takeLine :: ByteString.ByteString -> Maybe (ByteString.ByteString, ByteString.ByteString)
takeLine b =
 case Char8.elemIndex '\n' b of
  Nothing -> Nothing
  Just ix -> Just $ Char8.splitAt (ix + 1) b

