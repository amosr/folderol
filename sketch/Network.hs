{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Network where

-- data S a = IO a

-- Synchronous dataflow:
-- (+) :: S (Int, Int) -> S Int
-- split and join

-- Pull combinator:
-- zip :: (S a, S b) -> S (a, b)
-- join
-- Another possibility:

data S' a = (a -> IO ()) -> IO ()

data Pull2 i1 i2 o
 = Out o (Pull2 i1 i2 o)
 | In1 (i1 -> Pull2 i1 i2 o)
 | In2 (i2 -> Pull2 i1 i2 o)
-- zip :: (Either (a -> 

-- Push-like (co-pull?):
-- unzip :: S (a, b) -> S (Either a b)
-- split

-- Flowchart?
-- ? :: Either (S a) (S b) -> Either (S c) (S d)
-- split and join?

class Network n where
 type Stream n :: * -> *
 type Stream2 n :: * -> * -> *

-- Ramification constants:
class Network n => Copy n where
  copy :: Stream n a -> Stream2 n a a
-- Might be interesting if we had linear types
class Network n => Sink n where
  sink :: Stream n a -> ()


-- Branching constants:
-- Apparently only works if two inputs are equal but that's weird
-- For push streams merge seems reasonable.
-- For synchronous, why not tuple together (Stream2 n a b -> Stream n (a,b)).
-- For pull, could append or intersperse, or could zip if return was tuple...
class Network n => EqualityTest n where
  equals :: Stream2 n a a -> Stream n a

-- Nothing ever happens
-- Might be useful for push and pull and flowchart, not applicable to synchronous?
class Network n => Source n where
  source :: Stream n a

