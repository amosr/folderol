{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
module Folderol.Source where

import P

import qualified Data.Vector.Generic as Generic

data Source m a
 = forall s
 . Source
 { init :: m s
 , pull :: s -> m (Maybe a, s)
 , done :: s -> m ()
 }

{-# INLINE sourceRepeat #-}
sourceRepeat :: Monad m => Maybe a -> Source m a
sourceRepeat a
 = Source 
 { init = return ()
 , pull = \() -> return (a, ())
 , done = \() -> return ()
 }

{-# INLINE sourceOfList #-}
sourceOfList :: Monad m => [a] -> Source m a
sourceOfList as0
 = Source 
 { init = return as0
 , pull = \as -> case as of
    []      -> return (Nothing, [])
    (a:as') -> return (Just a, as')
 , done = \_  -> return ()
 }

{-# INLINE sourceOfVector #-}
sourceOfVector :: (Monad m, Generic.Vector v a) => v a -> Source m a
sourceOfVector !as0
 = Source 
 { init = return 0
 , pull = \ix -> if ix >= Generic.length as0
                 then return (Nothing, ix)
                 else return (Just $ Generic.unsafeIndex as0 ix, ix + 1)
 , done = \_  -> return ()
 }

-- | SourceOfVector with the branches flipped.
-- Used for benchmarking.
-- Originally had this version, but it turns out flipping the branches makes it 25% faster or so,
-- because this version constructs a loop with the end case in the middle.
{-# INLINE sourceOfVectorFlip #-}
sourceOfVectorFlip :: (Monad m, Generic.Vector v a) => v a -> Source m a
sourceOfVectorFlip !as0
 = Source 
 { init = return 0
 , pull = \ix -> if ix < Generic.length as0
                 then return (Just $ Generic.unsafeIndex as0 ix, ix + 1)
                 else return (Nothing, ix)
 , done = \_  -> return ()
 }

