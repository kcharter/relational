{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

{-|

A type class for relational data, 'Relational'.

The class is meant to be abstract enough that it can be implemented by
in-memory data structures, by interactions with a relational database,
or by the AST for a language of relational expressions.

-}

module Relational.Class where

import Control.Monad.Error (MonadError)

import Relational.ColName (ColName)
import Relational.Condition
import Relational.Error

-- | Relational types. A relational type supports the six fundamental
-- operations of the relational algebra: union, difference, attribute
-- renaming, projection, selection, and either cartesian product or
-- (theta) join.
--
-- A minimal implementation includes 'signature', 'tuples', 'make',
-- 'union', 'difference', 'rename', 'project', 'select' and either
-- 'join' or 'cartesianProduct'. A more sophisticated implementation
-- that attempts to reduce the work done in 'join' will probably
-- implement both 'join' and 'cartesianProduct'.
class (Ord d, MonadError RelationalError m) => MonadRelational d r m | m -> r, r -> d where
    -- | Extracts the signature of a relation as a list of
    -- attribute names.
    signature :: r -> m [ColName]
    -- | Extracts the tuples of a relation, as a list of lists of
    -- data values. Each data list must have the same length as
    -- in 'signature', and the data must be in the same order as
    -- the attribute names in 'signature'.
    tuples :: r -> m [[d]]
    -- | Makes a relational value with a given signature and
    -- given tuples. Implementations should throw errors if the list of
    -- attribute names contains duplicates, or if any of
    -- the tuples is the wrong length. It is not an
    -- error for the tuple list to contain duplicates, although
    -- the duplicates must appear only once in the tuples of
    -- the result.
    make :: [ColName] -> [[d]] -> m r
    -- | Computes the relational union of two relational
    -- values. The relational values must have equal signatures.
    union :: r -> r -> m r
    -- | Computes the relational difference of two relational
    -- values. The relational values must have equal signatures.
    difference :: r -> r -> m r
    -- | Computes the relational intersection of two relational
    -- values. The relational values must have equal signatures.
    -- The default implementation uses 'difference'.
    intersection :: r -> r -> m r
    intersection r s = (r `difference`) =<< (r `difference` s)
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
    rename :: ColName -> ColName -> r -> m r
    -- | Computes a new relational value that contains a subset of
    -- the attributes of another relational value.
    --
    -- In @project names r@, it is an error if any of @names@ is not
    -- in the signature of @r@. @names@ may contain duplicates, but
    -- the final signature contains each name only once.
    project :: [ColName] -> r -> m r
    -- | Computes a new relational value with the same signature as an
    -- existing one, but with a subset of the existing tuples. The
    -- subset comprises exactly those tuples that pass a given
    -- condition.
    select :: Condition d m -> r -> m r
    -- | Computes a new relational value equivalent to the Cartesian
    -- product of two others, followed by a selection. The default
    -- implementation performs a 'select' on the result of
    -- 'cartesianProduct'. However, implementations may attempt to
    -- split the condition into parts specific to the argument
    -- relational values, and then perform selections on the arguments
    -- before doing a Cartesian product.
    --
    -- In @join c r s@, it is an error if the signatures of @r@ and
    -- @s@ overlap.
    join :: Condition d m -> r -> r -> m r
    join c x y = cartesianProduct x y >>= select c
    -- | Computes the relational Cartesian product of two relational
    -- values. The signature of the result is the union of the two
    -- input signatures, and the tuple set contains every
    -- concatenation of tuples from the input relations. This method
    -- is logically equivalent to @'join' 'CondTrue'@, and this is the
    -- default implementation.
    --
    -- In @cartesianProduct r s@, it is an error if the signatures of
    -- @r@ and @s@ overlap.
    cartesianProduct :: r -> r -> m r
    cartesianProduct = join CondTrue

  