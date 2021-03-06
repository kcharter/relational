{-# LANGUAGE FlexibleContexts #-}

{-|

Signatures for relations. These are sets of attribute names with a
smaller interface.

This module is meant to be imported qualified, since it contains
functions whose names clash with list functions in the Prelude. -}

module Relational.Naive.Signature (Signature, fromList, safeFromList,
                                   toList, empty, null, size,
                                   contains, disjoint, union, intersection) where

import Prelude hiding (null)
import Control.Monad (liftM, foldM, when)
import Control.Monad.Error (MonadError)
import Data.List (intercalate)
import qualified Data.Set as S

import Relational.ColName
import Relational.Error (RelationalError, duplicatedColName)

newtype Signature = Signature (S.Set ColName) deriving (Eq, Ord)

instance Show Signature where
    show (Signature names) =
        "{" ++ intercalate "," (map show (S.toList names)) ++ "}"

fromList :: (ToColName a) => [a] -> Signature
fromList = Signature . S.fromList . map colName

safeFromList :: (ToColName a, MonadError RelationalError m) => [a] -> m Signature
safeFromList = (Signature `liftM`) . foldM addName S.empty 
    where addName soFar n =
              when (S.member n' soFar) (duplicatedName n' soFar) >> return (S.insert n' soFar)
              where n' = colName n
          duplicatedName n soFar = duplicatedColName n (S.toList soFar)

toSet :: Signature -> S.Set ColName
toSet (Signature s) = s

toList :: Signature -> [ColName]
toList = S.toList . toSet

empty :: Signature
empty = Signature S.empty

size :: Signature -> Int
size = liftSet S.size

null :: Signature -> Bool
null = liftSet S.null

contains :: (ToColName a) => a -> Signature -> Bool
contains = liftSet . S.member . colName

union :: Signature -> Signature -> Signature
union s = Signature . liftSet2 S.union s

intersection :: Signature -> Signature -> Signature
intersection s = Signature . liftSet2 S.intersection s

disjoint :: Signature -> Signature -> Bool
disjoint s1 s2 = null (s1 `intersection` s2)

liftSet :: (S.Set ColName -> a) -> Signature -> a
liftSet = (. toSet)

liftSet2 :: (S.Set ColName -> S.Set ColName -> a) -> Signature -> Signature -> a
liftSet2 f s r = f (toSet s) (toSet r)

