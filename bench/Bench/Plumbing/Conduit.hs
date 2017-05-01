{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Plumbing.Conduit where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Conduit  as C
import qualified System.IO as IO
import Control.Monad.Trans.Class (lift)


{-# INLINE sourceFile #-}
sourceFile :: FilePath -> C.Source IO ByteString.ByteString
sourceFile fp = do
  h <- lift $ IO.openFile fp IO.ReadMode
  go h
  lift $ IO.hClose h
 where
  go h = do
    e <- lift $ IO.hIsEOF h
    case e of
     False -> do
      l <- lift $ Char8.hGetLine h
      C.yield l
      go h
     True -> do
      return ()
   

{-# INLINE sinkFile #-}
sinkFile :: FilePath -> C.Sink ByteString.ByteString IO ()
sinkFile fp = do
  h <- lift $ IO.openFile fp IO.WriteMode
  go h
  lift $ IO.hClose h
 where
  go h = do
    e <- C.await
    case e of
     Just l -> do
      lift $ Char8.hPutStrLn h l
      go h
     Nothing -> return ()


