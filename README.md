# Folderol
See also: bauble, gewgaw, trifle, trinket,...

Streaming library that supports arbitrary splits and joins.
Streaming computations are expressed as a Kahn process network, and processes are fused together with a variant of synchronised product.
Fusion occurs at compile time via Template Haskell.

The idea is to execute over arbitrary amounts of data, so unbounded buffers need to be outlawed - or at least made explicit.
The fusion algorithm guarantees that if two programs fuse together, communication between the two requires only bounded buffers.
So we can use the number of processes after fusion as a rough estimation of how many unbounded buffers.
By default we only allow one process, which means no unbounded buffers,

Sometimes you do want unbounded buffers though, in which case you can set the maximum process count higher.

## Doing two things at once

Sometimes you need to do multiple things to the same input.
Say we have a list of stocks, and want to find which ones we should buy more of, as well as which ones we should sell.
We could define two functions:

```haskell
isBuy :: Stock -> Bool
isBuy stock = price stock < 0

isSell :: Stock -> Bool
isSell stock = price stock > 0

performBuy :: Stock -> IO ()
performBuy = ...network call to exchange...

performSell :: Stock -> IO ()
performSell = ...network call to exchange...
```

A winning strategy.
Now we can loop over the stocks and buy and sell them:
```haskell
trade :: [Stock] -> IO ()
trade stocks = do
  let buys  = filter isBuy stocks
  mapM_ performBuy  buys

  let sells = filter isSell stocks
  mapM_ performSell sells
```

The problem here is that we need to loop over stocks twice: once to compute buys, once to compute sells.
This extra loop might take a significant amount of time if the list is large.
Even worse though, if we were using lazy IO to pull the stocks list off disk, we've just blown up the memory by looping over it twice.

Rewriting it to loop over the list only once isn't too hard, assuming buy and sell are exclusive.

```haskell
trade :: [Stock] -> IO ()
trade stocks = do
  mapM_ performBoth stocks
 where
  performBoth s
   | isBuy s
   = performBuy s
   | isSell s
   = performSell s
   | otherwise
   = return ()
```

It's not *hard*, but it isn't *nice* either.
We've basically had to hand-fuse the two separate computations together to get the right time and space behaviour.
We really want some way to transform the first one into the second.

```haskell
import qualified Folderol as F

trade :: [Stock] -> IO ()
trade = $$(F.fuseList_1_0 F.defaultFuseOptions $ \stocks -> do
    buys   <- F.filter [|| isBuy ||] stocks
    F.mapM_ [|| performBuy ||] buys

    sells  <- F.filter [|| isSell ||] stocks
    F.mapM_ [|| performSell ||] sells)
```

You could say this isn't nice either.
But it's only *syntactically* ugly, whereas the hand-fused version is *semantically* ugly.
Aside from the template haskell noise and the first line, which converts the input list to a stream, it is a direct translation of the original, ideal version. 


## Relaxing fusion constraints

Some things inherently require unbounded buffers, and so cannot be fused together.
When I say "bounded buffer" I really mean constant size: even if a buffer is only linear in the input size, it is considered "unbounded" in that a sufficiently large input would make it run out of memory.

Say you have an input stream, and you want to partition it into two streams: those above zero, and those below. 
This is fine.
However, if you want to do anything with the two parts such as append them together, this requires a buffer.
Consider if the entire input stream were less than zero: you would need to store the whole stream in memory until you got to the end, to know that you can start writing the second half of the append.

```haskell
 $$(fuseVector_1_1 defaultFuseOptions $ \inputs -> do
  aboves <- filter [|| (>=0) ||] inputs
  belows <- filter [||  (<0) ||] inputs
  append aboves belows)
```

If you try to compile this, it will give a warning:

```
Fused graph
-----------
()    ->{Folderol.Source.sourceOfVector invec_0}-> $0
$0    ->--------(dup2 / filter / filter)---------> $1 $2
$1 $2 ->----------------(append)-----------------> $3
$3    ->{Folderol.Sink.vectorOfChannel outref_0}-> ()

test/Test/Folderol/Kernel/PartitionAppend.hs:15:5: warning:
    Maximum process count exceeded: after fusion there are 2 processes, but maximum is 1
```

This says that it was able to fuse the two filters into a single process, along with the generated duplicator (dup2) that copies the input stream to both filters.
However, the append is still in a process on its own.

If you really want to run this, you can disable the warning by setting the maximum number of processes to two.
The process network will run with concurrent channels inserted instead of buffers.

```haskell
 $$(fuseVector_1_1 defaultFuseOptions { maximumProcessCount = Just 2 } $ \inputs -> do
  aboves <- filter [|| (>=0) ||] inputs
  belows <- filter [||  (<0) ||] inputs
  append aboves belows)
```


More or less the only thing you could do with both streams that does not require a buffer is performing a time/order-dependent merge - which would give the original stream.
However, this is in general a non-deterministic operation, and so is not supported here.

