{- |
Module      : Bio.Motions.Callback.Class
Description : Contains the definitions of various 'Callback'-related primitives.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
module Bio.Motions.Callback.Class where

import Bio.Motions.Types
import Bio.Motions.Representation.Class

-- |Represents a callback
--
-- 'm' denotes a 'Monad' (or 'Applicative') in which the callback
-- is willing to operate.
class Callback m cb where
    -- |Computes the callback's result from scratch.
    runCallback :: Representation m repr 
        => repr 
        -- ^The representation.
        -> m cb
        -- ^The computed value.

    -- |Computes the callback's result after a move.
    updateCallback :: Representation m repr
        => repr
        -- ^The representation before making the move.
        -> cb
        -- ^The previous value.
        -> Move
        -- ^A move that is about to be made.
        -> m cb
        -- ^The new value.

-- |A convenient existential wrapper around a 'Callback' running in a 'Monad' 'm'
--
-- The result of the callback is required to be 'Show'able due to the need of
-- serialization. TODO: Create a better serializability constraint.
data CallbackWrapper m where
    CallbackWrapper :: (Callback m cb, Show cb) => cb -> CallbackWrapper m

-- |An alias for a particularily important class of callbacks, viz. score functions.
type Score m cb = (Callback m cb, Num cb, Ord cb)
