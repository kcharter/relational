{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Relational.Class where

import Control.Monad.Error (Error, MonadError)

-- | Selection conditions, used in selections and joins.
-- This is an abstract syntax for a simple language of boolean
-- conditions on tuples. In practice, the third type parameter
-- should be some kind of error monad.
data Condition n d m =
    CondTrue |
    -- ^ The true constant.
    CondFalse |
    -- ^ The false constant.
    CondNot (Condition n d m) |
    -- ^ Logical not.
    CondAnd (Condition n d m) (Condition n d m) |
    -- ^ Logical and.
    CondOr (Condition n d m) (Condition n d m) |
    -- ^ Logical or.
    CondRel RelOp (Expression n d m) (Expression n d m) |
    -- ^ A relational test on two data expressions.
    CondTupleTest ((n -> m d) -> m Bool)
    -- ^ Holds an arbitrary boolean-valued test.
    -- The test takes a lookup function that maps attribute
    -- names to values in 'm' and returns a boolean in 'm'.

-- | The fundamental relational operators on attribute values.
data RelOp = RelLT | RelEq | RelGT deriving (Eq, Ord, Show)

-- | An expression that produces a data value from an implicit tuple.
data Expression n d m =
    ExpConst d |
    -- ^ A constant data value.
    ExpValueOf n |
    -- ^ The value of the named attribute on the tuple.
    ExpCall ([d] -> m d) [Expression n d m]
    -- ^ The result of calling a function on a list of argument
    -- expressions.

-- | Relational types. A relational type supports the six fundamental
-- operations of the relational algebra: union, difference, attribute
-- renaming, projection, selection, and either cartesian product or
-- (theta) join.
class (Ord n, Ord d) => Relational n d r | r -> n, r -> d where
    signature :: (Monad m) => r -> m [n]
    tuples :: (Monad m) => r -> m [[d]]
    make :: (Error e, MonadError e m) => [n] -> [[d]] -> m r
    union :: (Error e, MonadError e m) => r -> r -> m r
    difference :: (Error e, MonadError e m) => r -> r -> m r
    rename :: (Error e, MonadError e m) => n -> n -> r -> m r
    project :: (Error e, MonadError e m) => [n] -> r -> m r
    select :: (Error e, MonadError e m) => Condition n d m -> r -> m r
    join :: (Error e, MonadError e m) => Condition n d m -> r -> r -> m r
    join c x y = cartesianProduct x y >>= select c
    cartesianProduct :: (Error e, MonadError e m) => r -> r -> m r
    cartesianProduct = join CondTrue

    