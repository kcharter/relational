{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveFunctor #-}

{-|

A simple in-memory implementation of relations. This is intended
for unit tests and prototypes.

-}

module Relational.Naive (Relation, RelationalMonad(..), RelationalMonadT(..)) where

import Control.Monad (when, unless, liftM, foldM, filterM, liftM2)
import Control.Monad.Error (MonadError, ErrorT)
import Control.Monad.Trans (MonadTrans, MonadIO)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Relational.Class
import Relational.ColName
import Relational.Condition
import Relational.Error
import Relational.Naive.Error
import qualified Relational.Naive.Signature as Sig

-- | A monad for pure, in-memory relational computations.
newtype RelationalMonad d a =
  RelationalMonad { runRel :: Either RelationalError a }
  deriving (Monad, Functor, MonadError RelationalError)

-- | A monad transformer that adds in-memory relational computations.
newtype RelationalMonadT d m a =
  RelationalMonadT { runRelT :: ErrorT RelationalError m a }
  deriving (Monad, Functor, MonadError RelationalError, MonadTrans, MonadIO)
           
data Relation a = Relation { relSig :: Sig.Signature,
                             relTupleSet :: S.Set (V.Vector a) } deriving (Eq, Ord, Show)

relSignature :: Relation a -> [ColName]
relSignature = Sig.toList . relSig

relTuples :: Relation a -> [[a]]
relTuples = map V.toList . S.toList . relTupleSet

relEmpty :: Sig.Signature -> Relation a
relEmpty s = Relation { relSig = s, relTupleSet = S.empty }

relUnsafeAddTuple :: (Ord a, MonadError RelationalError m) => [ColName] -> [a] -> Relation a -> m (Relation a)
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
              V.fromList . M.elems . M.fromList . zip names


relMake :: (Ord a, MonadError RelationalError m) => [ColName] -> [[a]] -> m (Relation a)
relMake names tuples =
    do sig <- Sig.safeFromList names
       foldM (flip (relUnsafeAddTuple names)) (relEmpty sig) tuples

relUnion :: (Ord a, MonadError RelationalError m) => Relation a -> Relation a -> m (Relation a)
relUnion r s =
    do relCheckEqualSignatures r s
       return Relation { relSig = relSig r,
                         relTupleSet = S.union (relTupleSet r) (relTupleSet s) }


relDifference :: (Ord a, MonadError RelationalError m) => Relation a -> Relation a -> m (Relation a)
relDifference r s =
    do relCheckEqualSignatures r s
       return Relation { relSig = relSig r,
                         relTupleSet = S.difference (relTupleSet r) (relTupleSet s) }

relCheckEqualSignatures :: (MonadError RelationalError m) => Relation a -> Relation a -> m ()
relCheckEqualSignatures r s =
    when (rSig /= sSig) signatureMismatch
    where rSig = relSig r
          sSig = relSig s
          signatureMismatch =
              die ("Signature mismatch: " ++
                   show rSig ++
                   " versus " ++
                   show sSig ++ ".")

relRename :: (Ord a, MonadError RelationalError m) => ColName -> ColName -> Relation a -> m (Relation a)
relRename n m r =
    do unless (inSignature n) nNotInSignature
       (if m == n
        then return r
        else do when (inSignature m) mInSignature
                foldM (flip (relUnsafeAddTuple newAttrs)) (relEmpty newSig) (relTuples r))
    where rSig = relSig r
          inSignature name = Sig.contains name rSig
          nNotInSignature = die (show n ++ " is not in signature " ++ show rSig ++ ".")
          mInSignature = die (show m ++ " is already in signature " ++ show rSig ++ ".")
          -- TODO: There must already be a function for replacing an
          -- element of a list. Isn't there?
          newAttrs = let (fst, nRest) = break (n==) (Sig.toList rSig)
                     in fst ++ (m:tail nRest)
          newSig = Sig.fromList newAttrs
              

relProject :: (Ord a, MonadError RelationalError m) => [ColName] -> Relation a -> m (Relation a)
relProject names r =
    do mapM_ checkSigContains names
       foldM addNewTuple (relEmpty newSig) newTuples
    where checkSigContains n =
              unless (Sig.contains n sig) (die ("Attribute " ++ show n ++
                                                " is not in signature " ++ show sig ++ "."))
          addNewTuple = flip (relUnsafeAddTuple orderedNames)
          orderedNames = Sig.toList newSig
          newTuples = map dropValues (relTuples r)
          dropValues = map snd . filter fst . zip mask
          mask = map (flip Sig.contains newSig) oldNames
          newSig = Sig.fromList names
          oldNames = Sig.toList sig
          sig = relSig r

relSelect :: (Ord a, MonadError RelationalError m) => Condition a m -> Relation a -> m (Relation a)
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
          indexForName = M.fromList (zip (Sig.toList rSig) [0..])


relCartesianProduct :: (Ord a, MonadError RelationalError m) => Relation a -> Relation a -> m (Relation a)
relCartesianProduct r s =
    do unless disjointSigs overlappingSigs
       return (relPutTupleSet (relEmpty newSig) newTupleSet)
    where disjointSigs = Sig.null (Sig.intersection rSig sSig)
          rSig = relSig r
          sSig = relSig s
          overlappingSigs = die ("Signature " ++ show rSig ++
                                 " and signature " ++ show sSig ++
                                 " overlap.")
          newSig = Sig.union rSig sSig
          newTupleSet = S.fromList (liftM2 mergeTuples rTuples sTuples)
          mergeTuples tr ts =
              V.fromList $ M.elems $ M.fromList pairs
              where pairs = rPairs ++ sPairs
                    rPairs = mkPairs rNames tr
                    sPairs = mkPairs sNames ts
                    mkPairs names = zip names . V.toList
          rNames = Sig.toList rSig
          sNames = Sig.toList sSig
          rTuples = tupleList r
          sTuples = tupleList s
          tupleList = S.toList . relTupleSet

relPutTupleSet :: Relation a -> S.Set (V.Vector a) -> Relation a
relPutTupleSet r tupleSet = r {relTupleSet = tupleSet}

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
