{-|

Signatures for relations. These are sets of attribute names with a
smaller interface.

This module is meant to be imported qualified, since it contains
functions whose names clash with list functions in the Prelude. -}

module Relational.Naive.Signature (Signature, fromList, safeFromList,
                                   toList, empty, null, size,
                                   contains, union, intersection) where

import Prelude hiding (null)
import Control.Monad (liftM, foldM, when)
import Control.Monad.Error (Error, MonadError)
import Data.List (intercalate)
import qualified Data.Set as S

import Relational.Naive.AttrName
import Relational.Naive.Error

newtype Signature = Signature (S.Set AttrName) deriving (Eq, Ord)

instance Show Signature where
    show (Signature names) =
        "{" ++ intercalate "," (map show (S.toList names)) ++ "}"

fromList :: (ToAttrName a) => [a] -> Signature
fromList = Signature . S.fromList . map toAttrName

safeFromList :: (ToAttrName a, Error e, MonadError e m) => [a] -> m Signature
safeFromList = (Signature `liftM`) . foldM addName S.empty 
    where addName soFar n =
              when (S.member n' soFar) (duplicateName n') >> return (S.insert n' soFar)
              where n' = toAttrName n
          duplicateName n = die ("'" ++ n' ++ "' appears more than once.")
              where n' = fromAttrName n

toSet :: Signature -> S.Set AttrName
toSet (Signature s) = s

toList :: (FromAttrName a) => Signature -> [a]
toList = map fromAttrName . S.toList . toSet

empty :: Signature
empty = Signature S.empty

size :: Signature -> Int
size = liftSet S.size

null :: Signature -> Bool
null = liftSet S.null

contains :: (ToAttrName a) => a -> Signature -> Bool
contains = liftSet . S.member . toAttrName

union :: Signature -> Signature -> Signature
union s = Signature . liftSet2 S.union s

intersection :: Signature -> Signature -> Signature
intersection s = Signature . liftSet2 S.intersection s

liftSet :: (S.Set AttrName -> a) -> Signature -> a
liftSet = (. toSet)

liftSet2 :: (S.Set AttrName -> S.Set AttrName -> a) -> Signature -> Signature -> a
liftSet2 f s r = f (toSet s) (toSet r)

