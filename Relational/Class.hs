{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

{-|

A type class for relational data, 'Relational'.

The class is meant to be abstract enough that it can be implemented by
in-memory data structures, by interactions with a relational database,
or by the AST for a language of relational expressions.

-}

module Relational.Class where

import Control.Monad.Error (Error, MonadError)

import Relational.Condition

-- | Relational types. A relational type supports the six fundamental
-- operations of the relational algebra: union, difference, attribute
-- renaming, projection, selection, and either cartesian product or
-- (theta) join.
class (Ord n, Ord d) => Relational n d r | r -> n, r -> d where
    -- | Extracts the signature of a relation as a list of
    -- attribute names.
    signature :: (Monad m) => r -> m [n]
    -- | Extracts the tuples of a relation, as a list of lists of
    -- data values. Each data list must have the same length as
    -- in 'signature', and the data must be in the same order as
    -- the attribute names in 'signature'.
    tuples :: (Monad m) => r -> m [[d]]
    -- | Makes a relational value with a given signature and
    -- given tuples. Implementations should throw errors if the list of
    -- attribute names contains duplicates, or if any of
    -- the tuples is the wrong length. It is not an
    -- error for the tuple list to contain duplicates, although
    -- the duplicates must appear only once in the tuples of
    -- the result.
    make :: (Error e, MonadError e m) => [n] -> [[d]] -> m r
    -- | Computes the relational union of two relational
    -- values. The relational values must have equal signatures.
    union :: (Error e, MonadError e m) => r -> r -> m r
    -- | Computes the relational difference of two relational
    -- values. The relational values must have equal signatures.
    difference :: (Error e, MonadError e m) => r -> r -> m r
    -- | Renames a single attribute. The result is the same as the
    -- input relational value, except one attribute name has
    -- disappeared and been replaced with another. The new attribute
    -- name is now associated with the original attribute's data
    -- values. Implementations may reorder the 'signature' and the
    -- 'tuples'.
    --
    -- In @rename n m r@, it is an error if @n@ is not in @r@'s
    -- signature, and it is also an error if @m@ is already in @r@'s
    -- signature, unless @n == m@.
    rename :: (Error e, MonadError e m) => n -> n -> r -> m r
    -- | Computes a new relational value that contains a subset of
    -- the attributes of another relational value.
    --
    -- In @project names r@, it is an error if any of @names@ is not
    -- in the signature of @r@. @names@ may contain duplicates, but
    -- the final signature contains each name only once.
    project :: (Error e, MonadError e m) => [n] -> r -> m r
    -- | Computes a new relational value with the same signature as an
    -- existing one, but with a subset of the existing tuples. The
    -- subset comprises exactly those tuples that pass a given
    -- condition.
    select :: (Error e, MonadError e m) => Condition n d m -> r -> m r
    join :: (Error e, MonadError e m) => Condition n d m -> r -> r -> m r
    join c x y = cartesianProduct x y >>= select c
    cartesianProduct :: (Error e, MonadError e m) => r -> r -> m r
    cartesianProduct = join CondTrue

    