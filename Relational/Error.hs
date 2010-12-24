-- | Special-purpose errors for relational operations. The
-- 'RelationalError' type is an instance of 'Error' and captures
-- common errors in relational operations.

module Relational.Error where

import Control.Monad.Error (Error(..))

import Relational.ColName
import Relational.RelName

-- | Special-purpose errors for relational operations.
data RelationalError =
    DuplicatedName ColName |
    -- ^ A name is duplicated in 'make'.
    NoSuchName ColName |
    -- ^ An expression references an attribute that doesn't exist.
    DifferingSignatures [ColName] [ColName] |
    -- ^ Two signatures differ, and so are not union-compatible.
    OverlappingSignatures [ColName] [ColName] |
    -- ^ Two signatures are not disjoint, and so are not product-compatible.
    NoSuchRelName RelName |
    -- ^ Reference to a non-existent relation name.
    GeneralError String
    -- ^ A catch-all for all other errors.
    deriving (Eq, Ord, Show)

instance Error RelationalError where
    strMsg = GeneralError

