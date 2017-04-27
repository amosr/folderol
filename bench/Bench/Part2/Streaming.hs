module Bench.Part2.Streaming where

import qualified Streaming.Prelude as S
import qualified System.IO as IO

runPart2 :: FilePath -> FilePath -> FilePath -> IO (Int, Int)
runPart2 in1 out1 out2 = do
  i1 <- IO.openFile in1  IO.ReadMode
  o1 <- IO.openFile out1 IO.WriteMode
  o2 <- IO.openFile out2 IO.WriteMode
  (i S.:> j S.:> _)  <- go i1 o1 o2
  IO.hClose i1
  IO.hClose o1
  IO.hClose o2
  return (i,j)
 where
  go i1 o1 o2
   = into        prd  o1
   $ into (not . prd) o2
   $ S.copy
   $ S.fromHandle i1

  into p o i
   = S.toHandle o
   $ S.store S.length
   $ S.filter p i

  prd l = length l `mod` 2 == 0



