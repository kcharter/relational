module RelationalProps where

import Control.Monad (liftM, liftM2, join)
import Control.Monad.Error (Error, MonadError)
import qualified Data.Map as DM
import qualified Data.Set as DS

import qualified Relational.Class as R

prop_makeSigAndTuples :: (R.Relational n d r) => ([n] -> [[d]] -> Either String r) -> ([n], [[d]]) -> Bool
prop_makeSigAndTuples mk = noErr . propM_makeSigAndTuples mk

propM_makeSigAndTuples :: (R.Relational n d r, Error e, MonadError e m) =>
                          ([n] -> [[d]] -> m r) -> ([n],[[d]]) -> m Bool
propM_makeSigAndTuples mk (names, tuples) =
    do r <- mk names tuples
       names' <- R.signature r
       tuples' <- R.tuples r
       return (toSetOfMaps names tuples == toSetOfMaps names' tuples')
    where toSetOfMaps names tuples =
              DS.fromList (map (DM.fromList . zip names) tuples)



prop_canRenameExistingToItself :: (R.Relational n d r, Eq r) => r -> Bool
prop_canRenameExistingToItself = noErr . propM_canRenameExistingToItself

propM_canRenameExistingToItself :: (R.Relational n d r, Eq r, Error e, MonadError e m) => r -> m Bool
propM_canRenameExistingToItself r =
    do s <- R.signature r
       (if null s
        then return True
        else let n = head s
             in (r==) `liftM` R.rename n n r)

prop_renameIsReversible :: (R.Relational n d r, Eq r) => (r, n) -> Bool
prop_renameIsReversible = noErr . propM_renameIsReversible

propM_renameIsReversible :: (R.Relational n d r, Eq r, Error e, MonadError e m) => (r, n) -> m Bool
propM_renameIsReversible (r, n) =
    do s <- R.signature r
       (if null s || n `elem` s
        then return True
        else let n' = head s
             in do r' <- R.rename n' n r
                   r'' <- R.rename n n' r'
                   return (r' == r''))

prop_canRemoveIntermediateRenames :: (R.Relational n d r, Eq r) => (r, n, n) -> Bool
prop_canRemoveIntermediateRenames = noErr . propM_canRemoveIntermediateRenames

propM_canRemoveIntermediateRenames :: (R.Relational n d r, Eq r, Error e, MonadError e m) =>
                                      (r, n, n) -> m Bool
propM_canRemoveIntermediateRenames (r, n, m) =
    do s <- R.signature r
       (if null s || n `elem` s || m `elem` s
        then return True
        else do r' <- R.rename (head s) n r
                r'' <- R.rename n m r'
                r''' <- R.rename (head s) m r
                return (r' == r'''))
               

prop_unionWithSelfIsSelf :: (R.Relational n d r, Eq r) => r -> Bool
prop_unionWithSelfIsSelf = noErr . propM_unionWithSelfIsSelf

propM_unionWithSelfIsSelf :: (R.Relational n d r, Eq r, Error e, MonadError e m) =>
                             r -> m Bool
propM_unionWithSelfIsSelf r =
    (r==) `liftM` R.union r r

prop_unionWithEmptyIsSelf :: (R.Relational n d r, Eq r) => r -> Bool
prop_unionWithEmptyIsSelf = noErr . propM_unionWithEmptyIsSelf

propM_unionWithEmptyIsSelf :: (R.Relational n d r, Eq r, Error e, MonadError e m) =>
                              r -> m Bool
propM_unionWithEmptyIsSelf r =
    (r==) `liftM` (R.union r =<< emptyLike r)

prop_unionIsCommutative :: (R.Relational n d r, Eq r) => (r, r) -> Bool
prop_unionIsCommutative = noErr . propM_unionIsCommutative

propM_unionIsCommutative :: (R.Relational n d r, Eq r, Error e, MonadError e m) =>
                            (r, r) -> m Bool
propM_unionIsCommutative = propM_commutative R.union

prop_unionIsAssociative :: (R.Relational n d r, Eq r) => (r, r, r) -> Bool
prop_unionIsAssociative = noErr . propM_unionIsAssociative

propM_unionIsAssociative :: (R.Relational n d r, Eq r, Error e, MonadError e m) =>
                            (r, r, r) -> m Bool
propM_unionIsAssociative = propM_associative R.union

prop_unionLikeSetUnion :: (R.Relational n d r) => (r, r) -> Bool
prop_unionLikeSetUnion = noErr . propM_unionLikeSetUnion

propM_unionLikeSetUnion :: (R.Relational n d r, Error e, MonadError e m) => (r, r) -> m Bool
propM_unionLikeSetUnion = propM_likeSetOp R.union DS.union

prop_differenceWithSelfIsEmpty :: (R.Relational n d r, Eq r) => r -> Bool
prop_differenceWithSelfIsEmpty = noErr . propM_differenceWithSelfIsEmpty

propM_differenceWithSelfIsEmpty :: (R.Relational n d r, Eq r, Error e, MonadError e m) => r -> m Bool
propM_differenceWithSelfIsEmpty r =
    eqM (R.difference r r) (emptyLike r)

prop_differenceWithEmptyIsSelf :: (R.Relational n d r, Eq r) => r -> Bool
prop_differenceWithEmptyIsSelf = noErr . propM_differenceWithEmptyIsSelf

propM_differenceWithEmptyIsSelf :: (R.Relational n d r, Eq r, Error e, MonadError e m) => r -> m Bool
propM_differenceWithEmptyIsSelf r =
    (r==) `liftM` (R.difference r =<< emptyLike r)

prop_differenceDeMorgan1 :: (R.Relational n d r, Eq r) => (r, r, r) -> Bool
prop_differenceDeMorgan1 = noErr . propM_differenceDeMorgan1

propM_differenceDeMorgan1 :: (R.Relational n d r, Eq r, Error e, MonadError e m) => (r, r, r) -> m Bool
propM_differenceDeMorgan1 (r, s, t) =
    eqM (R.difference r =<< R.intersection s t)
            (join (liftM2 R.union (R.difference r s) (R.difference r t)))


prop_differenceEmptyDiffAnyIsEmpty :: (R.Relational n d r, Eq r) => r -> Bool
prop_differenceEmptyDiffAnyIsEmpty = noErr . propM_differenceEmptyDiffAnyIsEmpty

propM_differenceEmptyDiffAnyIsEmpty :: (R.Relational n d r, Eq r, Error e, MonadError e m) => r -> m Bool
propM_differenceEmptyDiffAnyIsEmpty r =
    eqM (emptyLike r) (flip R.difference r =<< (emptyLike r))

prop_differenceDeMorgan2 :: (R.Relational n d r, Eq r) => (r, r, r) -> Bool
prop_differenceDeMorgan2 = noErr . propM_differenceDeMorgan1

propM_differenceDeMorgan2 :: (R.Relational n d r, Eq r, Error e, MonadError e m) => (r, r, r) -> m Bool
propM_differenceDeMorgan2 (r, s, t) =
    eqM (R.difference r =<< R.union s t)
            (join (liftM2 R.intersection (R.difference r s) (R.difference r t)))

-- the next "differencePM" properties are from the properties of set
-- difference as listed on PlanetMath at
-- http://planetmath.org/encyclopedia/PropertiesOfSetDifference.html

prop_differencePM7a :: (R.Relational n d r, Eq r) => (r, r) -> Bool
prop_differencePM7a = noErr . propM_differencePM7a

-- | property 7a: A - (A & B) = A - B
propM_differencePM7a :: (R.Relational n d r, Eq r, Error e, MonadError e m) => (r, r) -> m Bool
propM_differencePM7a (r, s) =
    eqM (R.difference r =<< (R.intersection r s)) (R.difference r s)

prop_differencePM7b :: (R.Relational n d r, Eq r) => (r, r) -> Bool
prop_differencePM7b = noErr . propM_differencePM7b

-- | property 7b: (A + B) - B = A - B
propM_differencePM7b :: (R.Relational n d r, Eq r, Error e, MonadError e m) => (r, r) -> m Bool
propM_differencePM7b (r, s) =
    eqM (flip R.difference s =<< (R.union r s)) (R.difference r s)

prop_differencePM8 :: (R.Relational n d r, Eq r) => (r, r, r) -> Bool
prop_differencePM8 = noErr . propM_differencePM8

-- | property 8: (A & B) - C = (A - C) & (B - C)
propM_differencePM8 :: (R.Relational n d r, Eq r, Error e, MonadError e m) => (r, r, r) -> m Bool
propM_differencePM8 (r, s, t) =
    eqM (flip R.difference t =<< (R.intersection r s))
            (join $ liftM2 R.intersection  (R.difference r t) (R.difference s t))

prop_differencePM9 :: (R.Relational n d r, Eq r) => (r, r, r) -> Bool
prop_differencePM9 = noErr . propM_differencePM9

-- | property 9: A & (B - C) = (A & B) - (A & C)
propM_differencePM9 :: (R.Relational n d r, Eq r, Error e, MonadError e m) => (r, r, r) -> m Bool
propM_differencePM9 (r, s, t) =
    eqM (R.intersection r =<< R.difference s t)
            (join $ liftM2 R.difference (R.intersection r s) (R.intersection s t))

prop_differencePM10 :: (R.Relational n d r, Eq r) => (r, r, r, r) -> Bool
prop_differencePM10 = noErr . propM_differencePM10

-- | property 10: (A - B) & (C - D) = (C - B) & (A - D)
propM_differencePM10 :: (R.Relational n d r, Eq r, Error e, MonadError e m) => (r, r, r, r) -> m Bool
propM_differencePM10 (r, s, t, u) =
    eqM (join $ liftM2 R.intersection (R.difference r s) (R.difference t u))
            (join $ liftM2 R.intersection (R.difference t s) (R.difference r u))

prop_differencePM11 :: (R.Relational n d r, Eq r) => (r, r, r, r) -> Bool
prop_differencePM11 = noErr . propM_differencePM11

-- | property 11: (A - B) & (C - D) = (A & B) - (B + D)
propM_differencePM11 :: (R.Relational n d r, Eq r, Error e, MonadError e m) => (r, r, r, r) -> m Bool
propM_differencePM11 (r, s, t, u) =
    eqM (join $ liftM2 R.intersection (R.difference r s) (R.difference t u))
            (join $ liftM2 R.difference (R.intersection r t) (R.union s u))

prop_differenceLikeSetDifference :: (R.Relational n d r) => (r, r) -> Bool
prop_differenceLikeSetDifference = noErr . propM_differenceLikeSetDifference

propM_differenceLikeSetDifference :: (R.Relational n d r, Error e, MonadError e m) => (r, r) -> m Bool
propM_differenceLikeSetDifference = propM_likeSetOp R.difference DS.difference

prop_intersectionWithSelfIsSelf :: (R.Relational n d r, Eq r) => r -> Bool
prop_intersectionWithSelfIsSelf = noErr . propM_intersectionWithSelfIsSelf

propM_intersectionWithSelfIsSelf :: (R.Relational n d r, Eq r, Error e, MonadError e m) => r -> m Bool
propM_intersectionWithSelfIsSelf r = (r==) `liftM` R.intersection r r

prop_intersectionWithEmptyIsEmpty :: (R.Relational n d r, Eq r) => r -> Bool
prop_intersectionWithEmptyIsEmpty = noErr . propM_intersectionWithEmptyIsEmpty

propM_intersectionWithEmptyIsEmpty :: (R.Relational n d r, Eq r, Error e, MonadError e m) => r -> m Bool
propM_intersectionWithEmptyIsEmpty r = (r==) `liftM` (R.intersection r =<< emptyLike r)

prop_intersectionIsCommutative :: (R.Relational n d r, Eq r) => (r, r) -> Bool
prop_intersectionIsCommutative = noErr . propM_intersectionIsCommutative

propM_intersectionIsCommutative :: (R.Relational n d r, Eq r, Error e, MonadError e m) => (r, r) -> m Bool
propM_intersectionIsCommutative = propM_commutative R.intersection

prop_intersectionIsAssociative :: (R.Relational n d r, Eq r) => (r, r, r) -> Bool
prop_intersectionIsAssociative = noErr . propM_intersectionIsAssociative

propM_intersectionIsAssociative :: (R.Relational n d r, Eq r, Error e, MonadError e m) => (r, r, r) -> m Bool
propM_intersectionIsAssociative = propM_associative R.intersection

prop_intersectionDistributesOverUnion :: (R.Relational n d r, Eq r) => (r, r, r) -> Bool
prop_intersectionDistributesOverUnion = noErr . propM_intersectionDistributesOverUnion

propM_intersectionDistributesOverUnion :: (R.Relational n d r, Eq r, Error e, MonadError e m) => (r, r, r) -> m Bool
propM_intersectionDistributesOverUnion (r, s, t) =
    eqM (R.intersection r =<< R.union s t)
            (join (liftM2 R.union (R.intersection r s) (R.intersection r t)))

prop_intersectionLikeSetIntersection :: (R.Relational n d r) => (r, r) -> Bool
prop_intersectionLikeSetIntersection = noErr . propM_intersectionLikeSetIntersection

propM_intersectionLikeSetIntersection :: (R.Relational n d r, Error e, MonadError e m) => (r, r) -> m Bool
propM_intersectionLikeSetIntersection = propM_likeSetOp R.intersection DS.intersection

emptyLike :: (R.Relational n d r, Error e, MonadError e m) => r -> m r
emptyLike r = R.signature r >>= flip R.make []

tupleSet :: (R.Relational n d r, Error e, MonadError e m) => r -> m (DS.Set [d])
tupleSet = liftM DS.fromList . R.tuples

propM_likeSetOp :: (R.Relational n d r, Error e, MonadError e m) =>
                   (r -> r -> m r)
                       -> (DS.Set [d] -> DS.Set [d] -> DS.Set [d])
                       -> (r, r) -> m Bool
propM_likeSetOp f sf (r, s) =
    eqM (f r s >>= tupleSet) (liftM2 sf (tupleSet r) (tupleSet s))

propM_commutative :: (Monad m, Eq a) => (a -> a -> m a) -> (a, a) -> m Bool
propM_commutative f (x,y) = eqM (f x y) (f y x)

propM_associative :: (Monad m, Eq a) => (a -> a -> m a) -> (a, a, a) -> m Bool
propM_associative f (x,y,z) =
    eqM (f x y >>= flip f z) (f x =<< f y z)

noErr :: Either String b -> Bool
noErr = either (const False) (const True)

eqM :: (Monad m, Eq a) => m a -> m a -> m Bool
eqM = liftM2 (==)
