-- | Special-purpose errors for relational operations. The
-- 'RelationalError' type is an instance of 'Error' and captures
-- common errors in relational operations.

module Relational.Error where

import Control.Monad.Error (Error(..))

import Relational.RelName

-- | Special-purpose errors for relational operations.
data RelationalError n =
    DuplicatedName n |
    -- ^ A name is duplicated in 'make'.
    NoSuchName n |
    -- ^ An expression references an attribute that doesn't exist.
    DifferingSignatures [n] [n] |
    -- ^ Two signatures differ, and so are not union-compatible.
    OverlappingSignatures [n] [n] |
    -- ^ Two signatures are not disjoint, and so are not product-compatible.
    NoSuchRelName RelName |
    -- ^ Reference to a non-existent relation name.
    GeneralError String
    -- ^ A catch-all for all other errors.
    deriving (Eq, Ord, Show)

instance Error (RelationalError n) where
    strMsg = GeneralError

