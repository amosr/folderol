{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Folderol.TradeExample where

import Folderol
import Folderol.Splice
import P hiding (filter, mapM_)

import Hedgehog

import System.IO

data Stock = Stock Int
 deriving Show

trade :: [Stock] -> IO ()
trade = $$(fuseList_1_0 defaultFuseOptions $ \stocks -> do
    buys   <- filter [|| isBuy ||] stocks
    mapM_ [|| performBuy ||] buys

    sells  <- filter [|| isSell ||] stocks
    mapM_ [|| performSell ||] sells)

performBuy :: Stock -> IO ()
performBuy s = putStrLn ("Buy " <> show s)

performSell :: Stock -> IO ()
performSell s = putStrLn ("Sell " <> show s)

isBuy :: Stock -> Bool
isBuy (Stock s) = s < 10

isSell :: Stock -> Bool
isSell (Stock s) = s > 10


tests :: IO Bool
tests = $$(checkConcurrent)
