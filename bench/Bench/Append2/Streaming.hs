module Bench.Append2.Streaming where

import Bench.Plumbing.Streaming
import qualified Streaming.Prelude as S

runAppend2 :: FilePath -> FilePath -> FilePath -> IO Int
runAppend2 in1 in2 out = do
  sinkFile out $ go (sourceFile in1) (sourceFile in2)
 where
  go s1 s2 = S.store S.length_ $ (s1 >> s2)

