{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
module Bench.Plumbing.Foldl where

import P hiding (count)

import qualified Control.Foldl as Fold
import Control.Foldl (Fold(..))

import qualified Data.Vector as Vector



-- Category composition for Folds - but we can't implement 'id' for a fold over empty input.
-- > fold (running after before) = fold after . postscan before
-- This is like a premap, but... it's even cooler
{-# INLINE running #-}
running :: Fold b c -> Fold a b -> Fold a c
running (Fold k1 z1 x1) (Fold k0 z0 x0) = Fold k' z' x'
 where
  k' (!s0,!s1) e = 
    let !s0' = k0 s0 e
        !e'  = x0 s0'
        !s1' = k1 s1 e'
    in (s0', s1')
        
  z' = (z0, z1)
  x' (_s0,!s1) = x1 s1

-- Close to id, but you need to give it a value in case there's no elements
-- > postscan (idiot x) xs =     xs
-- >     scan (idiot x) xs = x : xs
{-# INLINE idiot #-}
idiot :: a -> Fold a a
idiot z = Fold (\_ a -> a) z id

-- Delay an element by one.
-- > postscan (delay x) xs = x : init xs
--
-- It is a little weird because we need to emit a value for the initial (and empty stream case), as well as the first element.
-- So a scan actually includes two occurrences of the default element:
-- > scan (delay x) xs = x : x : init xs
{-# INLINE delay #-}
delay :: a -> Fold a a
delay def = Fold (\(!_,!b) c -> (b,c)) (def,def) fst

-- Construct a fold1 where the 'initial state' has access to the first element.
{-# INLINE fold1 #-}
fold1 :: (b -> a -> b) -> (a -> b) -> Fold a (Maybe b)
fold1 k z = Fold k' Nothing id
 where
  k' Nothing   x = Just $ k (z x) x
  k' (Just !s) x = Just $ k s x


-- Shift (subtract) each element by the first element of the stream 
-- > Fold.postscan shift [5,4,6,3,7]
-- > = [5 - 5, 4 - 5, 6 - 5, 3 - 5, 7 - 5]
-- > = [    0,    -1,     1,    -2,     2]
--
-- Because things like variance are "shift-invariant", the shifted variant is the same as the variant:
-- > running variance shift = variance
-- This can apparently give better numerical stability when the elements are large, because if the first element is anywhere near the mean of the samples, subtracting it will make the samples smaller.
{-# INLINE shift #-}
shift :: Num a => Fold a a
shift =
 let zero f = fromMaybe 0 <$> f
     repeats = zero $ fold1 const id
     stream  = zero $ Fold.last
 in  (-) <$> stream <*> repeats

{-# INLINE preshift #-}
preshift :: Num a => Fold a b -> Fold a b
preshift = flip running shift 

{-# INLINE zby0 #-}
zby0 :: Num b => Fold a b -> Fold a b
zby0 = running (delay 0)

{-# INLINE input #-}
input :: Num a => Fold a a
input = idiot 0

{-# INLINE count #-}
count :: Num b => Fold a b
count = Fold.genericLength

-- Numerically stable covariance stolen from Wikipedia: https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online
-- This is quite similar to the variance in Control.Foldl.
--
-- Sum of differences from the running mean.
-- There is a weird thing where we use the previous running mean for x, and current running mean for y. This is on purpose, but I don't entirely understand why.
--
-- C_n = C_n-1 + (x_n - mean x[0..n-1])(y_n - mean y[0..n])
{-# INLINE covariance #-}
covariance :: Fractional a => Fold (a,a) a
covariance =
  let dx   = input - zby0 Fold.mean
      dy   = input -          Fold.mean
      diff = Fold.premap fst dx * Fold.premap snd dy
  in running Fold.sum diff / count

{-# INLINE correlation #-}
correlation :: Floating a => Fold (a,a) a
correlation =
  covariance / (Fold.premap fst Fold.std * Fold.premap snd Fold.std)

-- In Icicle, Huw calls this "gradient with units y/x":
-- > gradient a b = covariance a b / variance b.
--
-- This is equivalent to
-- > correlation a b * variance a
-- So it's the correlation scaled by the magnitude of "a".
-- Which is like fitting the line to the points.
-- Finding the constant for the line is something like
-- > mean y - gradient y x * mean x
-- However, I need to double-check that.
{-# INLINE gradient #-}
gradient :: Floating a => Fold (a,a) a
gradient =
  covariance / Fold.premap snd Fold.variance


-- TODO: circular buffer maybe
{-# INLINE latest #-}
latest :: Int -> Fold a (Vector.Vector a)
latest n = Fold k z x
 where
  z = Vector.empty
  k !v !e = Vector.take n (Vector.cons e v)
  x !v = v

{-# INLINE groupByIndex #-}
groupByIndex :: Fold a b -> Fold (Vector.Vector a) (Vector.Vector b)
groupByIndex (Fold k0 z0 x0) = Fold k z x
 where
  z = Vector.empty
  k !vs !ve = 
    let !len = Vector.length vs `max` Vector.length ve
        get ix
         = case (vs Vector.!? ix, ve Vector.!? ix) of
            (Just s, Just e) -> k0 s e
            (Just s, Nothing) -> s
            (Nothing, Just e) -> k0 z0 e
            (Nothing, Nothing) -> z0
    in Vector.generate len get
  x = fmap x0

{-# INLINE crosscorrelation #-}
crosscorrelation :: Floating a => Int -> Fold (a,a) (Vector.Vector a)
crosscorrelation n =
  groupByIndex correlation
  `running`
  stutter2 n

{-# INLINE stutter2 #-}
stutter2 :: Int -> Fold (a,b) (Vector.Vector (a,b))
stutter2 n =
  go <$> Fold.premap fst Fold.last <*> Fold.premap snd (latest n)
 where
  go (Just a) bs = fmap ((,) a) bs
  go Nothing _ = Vector.empty


