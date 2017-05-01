module Bench.Part2.Streaming where

import Bench.Plumbing.Streaming
import qualified Streaming.Prelude as S
import qualified Data.ByteString as ByteString

runPart2 :: FilePath -> FilePath -> FilePath -> IO (Int, Int)
runPart2 in1 out1 out2 = do
  (i S.:> j S.:> _)  <- go
  return (i,j)
 where
  go
   = into        prd  out1
   $ into (not . prd) out2
   $ S.copy
   $ sourceFile in1

  into p o i
   = sinkFile o
   $ S.store S.length
   $ S.filter p i

  prd l = ByteString.length l `mod` 2 == 0

