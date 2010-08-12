{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|

A simple in-memory implementation of relations. This is intended
for unit tests and prototypes.

-}

module Relational.Naive (Signature, fromList, toList,
                         Relation) where

import Control.Monad (when, unless, liftM, foldM, filterM, liftM2)
import Control.Monad.Error (Error, MonadError)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Relational.Class
import Relational.Condition
import Relational.Naive.AttrName
import Relational.Naive.Error
import Relational.Naive.Signature

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

todo = error "Not implemented"

