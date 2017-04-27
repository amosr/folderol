module Bench.Append2.Conduit where

import qualified Data.Conduit  as C
import qualified System.IO as IO
import Control.Monad.Trans.Class (lift)


{-# INLINE sourceFile #-}
sourceFile :: FilePath -> C.Source IO String
sourceFile fp = do
  h <- lift $ IO.openFile fp IO.ReadMode
  go h
  lift $ IO.hClose h
 where
  go h = do
    e <- lift $ IO.hIsEOF h
    case e of
     False -> do
      l <- lift $ IO.hGetLine h
      C.yield l
      go h
     True -> do
      return ()
   

{-# INLINE sinkFile #-}
sinkFile :: FilePath -> C.Sink String IO ()
sinkFile fp = do
  h <- lift $ IO.openFile fp IO.WriteMode
  go h
  lift $ IO.hClose h
 where
  go h = do
    e <- C.await
    case e of
     Just l -> do
      lift $ IO.hPutStrLn h l
      go h
     Nothing -> return ()


runAppend2 :: FilePath -> FilePath -> FilePath -> IO Int
runAppend2 in1 in2 out =
  C.runConduit (sources C..| sinks)
 where
  sources = sourceFile in1 >> sourceFile in2

  sinks = do
   (i,_) <- C.fuseBoth (counting 0) (sinkFile out)
   return i

  counting i = do
   e <- C.await
   case e of
    Nothing   -> return i
    Just v -> do
     C.yield v
     counting (i + 1)
