{-# LANGUAGE FlexibleContexts #-}

-- | Special-purpose errors for relational operations. The
-- 'RelationalError' type is an instance of 'Error' and captures
-- common errors in relational operations.

module Relational.Error where

import Control.Monad.Error (MonadError(..), Error(..))

import Relational.ColName
import Relational.RelName

-- | Special-purpose errors for relational operations.
data RelationalError =
    DuplicatedColName ColName [ColName] |
    -- ^ A name is duplicated in 'make', or the target attribute name
    -- of a rename already exists.
    NoSuchColName ColName [ColName] |
    -- ^ An expression references an attribute that doesn't exist.
    DifferingSignatures [ColName] [ColName] |
    -- ^ Two signatures differ, and so are not union-compatible.
    OverlappingSignatures [ColName] [ColName] |
    -- ^ Two signatures are not disjoint, and so are not product-compatible.
    ArityMismatch Int [ColName] |
    -- ^ Given the wrong number of attributes for a tuple.
    NoSuchRelName RelName |
    -- ^ Reference to a non-existent relation name.
    GeneralError String
    -- ^ A catch-all for all other errors.
    deriving (Eq, Ord, Show)

instance Error RelationalError where
    strMsg = GeneralError

duplicatedColName :: (MonadError RelationalError m) => ColName -> [ColName] -> m a
duplicatedColName duped = throwError . DuplicatedColName duped

noSuchColName :: (MonadError RelationalError m) => ColName -> [ColName] -> m a
noSuchColName missing = throwError . NoSuchColName missing

differingSignatures :: (MonadError RelationalError m) => [ColName] -> [ColName] -> m a
differingSignatures names1 = throwError . DifferingSignatures names1

overlappingSignatures :: (MonadError RelationalError m) => [ColName] -> [ColName] -> m a
overlappingSignatures names1 = throwError . OverlappingSignatures names1

arityMismatch :: (MonadError RelationalError m) => Int -> [ColName] -> m a
arityMismatch valueCount = throwError . ArityMismatch valueCount

noSuchRelName :: (MonadError RelationalError m) => RelName -> m a
noSuchRelName = throwError . NoSuchRelName

generalError :: (MonadError RelationalError m) => String -> m a
generalError = throwError . GeneralError
