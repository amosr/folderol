{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Plumbing.Streaming where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified System.IO as IO
import qualified Streaming.Prelude as S
import           Control.Monad.IO.Class


{-# INLINE sourceFile #-}
sourceFile :: MonadIO m => FilePath -> S.Stream (S.Of ByteString.ByteString) m ()
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
      S.yield l
      go h
     True -> do
      return ()
   

{-# INLINE sinkFile #-}
sinkFile :: MonadIO m => FilePath -> S.Stream (S.Of ByteString.ByteString) m r -> m r
sinkFile fp str0 = do
  h <- liftIO $ IO.openFile fp IO.WriteMode
  r <- go h str0
  liftIO $ IO.hClose h
  return r
 where
  go h str = do
    e <- S.next str
    case e of
     Left r -> return r
     Right (l,str') -> do
      liftIO $ Char8.hPutStrLn h l
      go h str'

