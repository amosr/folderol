{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Append2.Folderol where

import Bench.Array.Folderol

import Folderol
import Folderol.Splice

import qualified Folderol.Source as Source
import qualified Folderol.Sink as Sink

import qualified System.IO as IO

sourceLinesOfFile :: FilePath -> Source.Source IO String
sourceLinesOfFile f = Source.Source
 { Source.init = IO.openFile f IO.ReadMode
 , Source.pull = \h -> do
    e <- IO.hIsEOF h
    case e of
     False -> do
      l <- IO.hGetLine h
      return (Just l, h)
     True -> do
      return (Nothing, h)
 , Source.done = \h -> do
    IO.hClose h
 }

sinkFileOfLines :: FilePath -> Sink.Sink IO String
sinkFileOfLines f = Sink.Sink
 { Sink.init = IO.openFile f IO.WriteMode
 , Sink.push = \h x -> do
    IO.hPutStrLn h x
    return h
 , Sink.done = \h -> do
    IO.hClose h
 }



runAppend2 :: FilePath -> FilePath -> FilePath -> IO Int
runAppend2 fpIn1 fpIn2 fpOut = do
  (count,()) <- scalarIO $ \(snkCount :: Sink.Sink IO Int) -> $$(fuse defaultFuseOptions $ do
    in1 <- source [|| sourceLinesOfFile fpIn1 ||]
    in2 <- source [|| sourceLinesOfFile fpIn2 ||]
    aps <- append in1 in2

    count <- fold [||\c _ -> c + 1||] [||0||] aps
    sink count [|| snkCount ||]
    sink aps   [|| sinkFileOfLines fpOut ||])
  return count

