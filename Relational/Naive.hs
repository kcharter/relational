{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

{-|

A simple in-memory implementation of relations. This is intended
for unit tests and prototypes.

-}

module Relational.Naive (AttrName,
                         ToAttrName(..),
                         FromAttrName(..),
                         Signature, fromList, toList,
                         Relation) where

import Control.Monad (when, unless, liftM, foldM, filterM, liftM2)
import Control.Monad.Error (Error, MonadError, strMsg, throwError)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Relational.Class
import Relational.Condition

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

toSet :: Signature -> S.Set AttrName
toSet (Signature s) = s

toList :: (FromAttrName a) => Signature -> [a]
toList = map fromAttrName . S.toList . toSet

size :: Signature -> Int
size = liftSet S.size

isEmpty :: Signature -> Bool
isEmpty = liftSet S.null

contains :: (ToAttrName a) => a -> Signature -> Bool
contains n = liftSet (S.member (toAttrName n))

-- TODO: move Signature to a separate module and rename this
sigUnion :: Signature -> Signature -> Signature
sigUnion s r = Signature (liftSet2 S.union s r)

intersection :: Signature -> Signature -> Signature
intersection s r = Signature (liftSet2 S.intersection s r)

liftSet :: (S.Set AttrName -> a) -> Signature -> a
liftSet f = f . toSet

liftSet2 :: (S.Set AttrName -> S.Set AttrName -> a) -> Signature -> Signature -> a
liftSet2 f s r = f (toSet s) (toSet r)

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

relDifference :: (Error e, MonadError e m, Ord a) => Relation a -> Relation a -> m (Relation a)
relDifference r s =
    do relCheckEqualSignatures r s
       return Relation { relSig = relSig r,
                         relTupleSet = S.difference (relTupleSet r) (relTupleSet s) }

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

relRename :: (Error e, MonadError e m, Ord a) => AttrName -> AttrName -> Relation a -> m (Relation a)
relRename n m r =
    do when (not (inSignature n)) nNotInSignature
       (if m == n
        then return r
        else do when (inSignature m) mInSignature
                foldM (flip (relUnsafeAddTuple newAttrs)) (relEmpty newSig) (relTuples r))
    where rSig = relSig r
          inSignature name = contains name rSig
          nNotInSignature = die (show n ++ " is not in signature " ++ show rSig ++ ".")
          mInSignature = die (show m ++ " is already in signature " ++ show rSig ++ ".")
          -- TODO: There must already be a function for replacing an
          -- element of a list. Isn't there?
          newAttrs = let (fst, nRest) = break (n==) (toList rSig)
                     in fst ++ (m:(tail nRest))
          newSig = fromList newAttrs
              
relProject :: (Error e, MonadError e m, Ord a) => [AttrName] -> Relation a -> m (Relation a)
relProject names r =
    do mapM_ checkSigContains names
       foldM addNewTuple (relEmpty newSig) newTuples
    where checkSigContains n =
              unless (contains n sig) (die ("Attribute " ++ show n ++
                                            " is not in signature " ++ show sig ++ "."))
          addNewTuple = flip (relUnsafeAddTuple orderedNames)
          orderedNames = toList newSig
          newTuples = map dropValues (relTuples r)
          dropValues = map snd . filter fst . (zip mask)
          mask = map (flip contains newSig) oldNames
          newSig = fromList names
          oldNames = toList sig :: [AttrName]
          sig = relSig r

relSelect :: (Error e, MonadError e m, Ord a) => Condition AttrName a m -> Relation a -> m (Relation a)
relSelect c r =
    relPutTupleSet (relEmpty rSig) `liftM` selectedTupleSet
    where rSig = relSig r
          selectedTupleSet = S.fromList `liftM` selectedTuples
          selectedTuples = filterM selector (S.toList (relTupleSet r))
          selector t = evalCondition (lookupFor t) c
          lookupFor t n = (t V.!) `liftM` indexFor n
          indexFor n =
              maybe noSuchName return (M.lookup n indexForName)
              where noSuchName = die ("No attribute " ++ show n ++
                                      " in signature " ++ show rSig ++ ".")
          indexForName = M.fromList (zip (toList rSig) [0..])

relCartesianProduct :: (Error e, MonadError e m, Ord a) => Relation a -> Relation a -> m (Relation a)
relCartesianProduct r s =
    do unless disjointSigs overlappingSigs
       return (relPutTupleSet (relEmpty newSig) newTupleSet)
    where disjointSigs = isEmpty (intersection rSig sSig)
          rSig = relSig r
          sSig = relSig s
          overlappingSigs = die ("Signature " ++ show rSig ++
                                 " and signature " ++ show sSig ++
                                 " overlap.")
          newSig = sigUnion rSig sSig
          newTupleSet = S.fromList (liftM2 mergeTuples rTuples sTuples)
          mergeTuples tr ts =
              V.fromList $ M.elems $ M.fromList pairs
              where pairs = rPairs ++ sPairs
                    rPairs = mkPairs rNames tr
                    sPairs = mkPairs sNames ts
                    mkPairs names tuple = zip names (V.toList tuple)
          rNames = toList rSig :: [AttrName]
          sNames = toList sSig :: [AttrName]
          rTuples = tupleList r
          sTuples = tupleList s
          tupleList = S.toList . relTupleSet

relPutTupleSet :: Relation a -> S.Set (V.Vector a) -> Relation a
relPutTupleSet r tupleSet = r {relTupleSet = tupleSet}

instance (Ord a) => Relational AttrName a (Relation a) where
    signature = return . relSignature
    tuples = return . relTuples
    make = relMake
    union = relUnion
    difference = relDifference
    rename = relRename
    project = relProject
    select = relSelect
    cartesianProduct = relCartesianProduct

die :: (Error e, MonadError e m) => String -> m a
die = throwError . strMsg

todo = error "Not implemented"

