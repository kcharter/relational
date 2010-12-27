{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveFunctor #-}

{-|

A simple in-memory implementation of relations. This is intended
for unit tests and prototypes.

-}

module Relational.Naive (Relation, RelationalMonad(..), RelationalMonadT(..)) where

import Control.Monad.Error (MonadError, ErrorT)
import Control.Monad.Trans (MonadTrans, MonadIO)

import Relational.Class
import Relational.Error
import Relational.Naive.Relation

-- | A monad for pure, in-memory relational computations.
newtype RelationalMonad d a =
  RelationalMonad { runRel :: Either RelationalError a }
  deriving (Monad, Functor, MonadError RelationalError)

-- | A monad transformer that adds in-memory relational computations.
newtype RelationalMonadT d m a =
  RelationalMonadT { runRelT :: ErrorT RelationalError m a }
  deriving (Monad, Functor, MonadError RelationalError, MonadTrans, MonadIO)
           

instance (Ord a) => MonadRelational a (Relation a) (RelationalMonad a) where
    signature = return . relSignature
    tuples = return . relTuples
    make = relMake
    union = relUnion
    difference = relDifference
    rename = relRename
    project = relProject
    select = relSelect
    cartesianProduct = relCartesianProduct
    
instance (Ord a, Monad m) => MonadRelational a (Relation a) (RelationalMonadT a m) where
    signature = return . relSignature
    tuples = return . relTuples
    make = relMake
    union = relUnion
    difference = relDifference
    rename = relRename
    project = relProject
    select = relSelect
    cartesianProduct = relCartesianProduct
  
todo = error "Not implemented"
