-- Models for generating shitty but plausible stock graphs, as well as simple analyses
module Bench.Correlation.Model where

import Bench.Plumbing.Foldl

import qualified Control.Foldl as Fold

type IFT = [(Double,Double,Double)]
-- dodgy inverse fourier type thing for adding some vaguely structured colour to the model
ift :: IFT -> [Double]
ift freqs = go 0
 where
  go i = sum (fmap (sc i) freqs) : go (i + 1)
  sc i (freq,ampc,amps) = cos (i * freq) * ampc + sin (i * freq) * amps

noise :: Double -> IFT
noise i = [(1, 0, i'), (2, i', 0), (3, 0, i'), (4, i', 0), (5, i' / 2, i' / 2)]
 where
  i' = i / 5

noiselo :: Double -> IFT
noiselo i = [(0.1, 0, i'), (0.3, i', 0), (0.5, 0, i'), (0.8, i', 0), (1.3, i' / 2, i' / 2)]
 where
  i' = i / 5


model :: Double -> Double -> IFT -> [Double]
model constant slope freqs = zipWith (+) line (ift freqs)
 where
  line = iterate (+ slope) constant

modelS c s lo hi = take 100
 $ model c s (noiselo lo ++ noise hi)

modelS1 :: [Double]
modelS1 = modelS 100 1 0 1

timed :: [a] -> [(Double,a)]
timed = zip [0..]

anal :: [(Double,Double)] -> IO ()
anal m = do
  pp m
  p "Correlation:   " $ f correlation m
  p "Covariance:    " $ f covariance m
  p "Variance X:    " $ f variance $ fmap fst m
  p "Variance Y:    " $ f variance $ fmap snd m
  p "Mean X:        " $ f mean $ fmap fst m
  p "Mean Y:        " $ f mean $ fmap snd m
  p "Gradient:      " $ f gradient m
  let mx = f mean $ fmap fst m
  let my = f mean $ fmap snd m
  let beta = f gradient m
  p "Y equation:    " ("y = " ++ show (my - beta * mx) ++ " + " ++ show beta ++ " x")
 where
  f = Fold.fold
  p h v = putStr h >> print v

  pp [] = putStrLn ""
  pp ((a,b):m') = do
   putStr $ show (truncate a, truncate b)
   putStr " "
   pp m'

