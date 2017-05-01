{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Bench.Plumbing.Pipes where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified System.IO as IO
import qualified Pipes as P
import           Control.Monad.IO.Class


{-# INLINE sourceFile #-}
sourceFile :: MonadIO m => FilePath -> P.Producer' ByteString.ByteString m ()
sourceFile fp = do
  h <- liftIO $ IO.openFile fp IO.ReadMode
  go h
  liftIO $ IO.hClose h
 where
  go h = do
    e <- liftIO $ IO.hIsEOF h
    case e of
     False -> do
      l <- liftIO $ Char8.hGetLine h
      P.yield l
      go h
     True -> do
      return ()
   

{-# INLINE sinkHandle #-}
sinkHandle :: MonadIO m => IO.Handle -> P.Consumer' ByteString.ByteString m r
sinkHandle h = P.for P.cat (\str -> liftIO (Char8.hPutStrLn h str))
