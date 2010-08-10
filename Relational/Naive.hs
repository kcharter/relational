{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

{-|

A simple in-memory implementation of relations. This is intended
for unit tests and prototypes.

-}

module Relational.Naive (AttrName,
                         ToAttrName, toAttrName,
                         FromAttrName, fromAttrName,
                         Signature, fromList, toList) where

import Control.Monad (when, liftM, foldM)
import Control.Monad.Error (Error, MonadError, strMsg, throwError)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Relational.Class

newtype AttrName = AttrName T.Text deriving (Eq, Ord)

instance Show AttrName where
    show (AttrName n) = T.unpack n

class ToAttrName a where
    toAttrName :: a -> AttrName

class FromAttrName a where
    fromAttrName :: AttrName -> a

instance ToAttrName AttrName where
    toAttrName = id

instance ToAttrName T.Text where
    toAttrName = AttrName

instance ToAttrName String where
    toAttrName = AttrName . T.pack

instance FromAttrName AttrName where
    fromAttrName = id

instance FromAttrName T.Text where
    fromAttrName (AttrName t) = t

instance FromAttrName String where
    fromAttrName (AttrName t) = T.unpack t

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

toList :: (FromAttrName a) => Signature -> [a]
toList (Signature s) = map fromAttrName (S.toList s)

size :: Signature -> Int
size (Signature s) = S.size s

data Relation a = Relation { relSig :: Signature,
                             relTupleSet :: S.Set (V.Vector a) } deriving (Eq, Ord, Show)

relSignature :: Relation a -> [AttrName]
relSignature = toList . relSig

relTuples :: Relation a -> [[a]]
relTuples = map V.toList . S.toList . relTupleSet

relEmpty :: Signature -> Relation a
relEmpty s = Relation { relSig = s, relTupleSet = S.empty }

relUnsafeAddTuple :: (Error e, MonadError e m, Ord a) => [AttrName] -> [a] -> Relation a -> m (Relation a)
relUnsafeAddTuple names values soFar =
    do when (nameCount /= valueCount) lengthMismatch
       return soFar{ relTupleSet = S.insert (mkTuple values) (relTupleSet soFar) }
    where nameCount = length names
          valueCount = length values
          lengthMismatch =
              die ("Given " ++ show valueCount ++
                   " values for " ++ show nameCount ++
                   " attribute names.")
          mkTuple =
              V.fromList . M.elems . M.fromList . (zip names)

relMake :: (Error e, MonadError e m, Ord a) => [AttrName] -> [[a]] -> m (Relation a)
relMake names tuples =
    do sig <- safeFromList names
       foldM (flip (relUnsafeAddTuple names)) (relEmpty sig) tuples

relUnion :: (Error e, MonadError e m, Ord a) => Relation a -> Relation a -> m (Relation a)
relUnion r s =
    do relCheckEqualSignatures r s
       return Relation { relSig = relSig r,
                         relTupleSet = S.union (relTupleSet r) (relTupleSet s) }
 
relCheckEqualSignatures :: (Error e, MonadError e m) => Relation a -> Relation a -> m ()
relCheckEqualSignatures r s =
    when (rSig /= sSig) signatureMismatch
    where rSig = relSig r
          sSig = relSig s
          signatureMismatch =
              die ("Signature mismatch: " ++
                   show rSig ++
                   " versus " ++
                   show sSig ++ ".")

instance (Ord a) => Relational AttrName a (Relation a) where
    signature = return . relSignature
    tuples = return . relTuples
    make = relMake
    union = relUnion
    difference = todo
    rename = todo
    project = todo
    select = todo
    join = todo

die :: (Error e, MonadError e m) => String -> m a
die = throwError . strMsg

todo = error "Not implemented"

