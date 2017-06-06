-- On the Duality of Streams
-- Jean-Philippe Bernardy, Josef Svenningsson
module Duality where

type N a = a -> IO ()
type NN a = N (N a)

data Source a = Nil  | Cons a (N (Sink   a))
data Sink   a = Full | Cont   (N (Source a))

type Src a = N (Sink   a)
type Snk a = N (Source a)

