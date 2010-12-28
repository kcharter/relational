module RelationalProps where

import Control.Monad (liftM, liftM2, join)
import Data.List (delete)
import Data.Maybe (catMaybes)
import qualified Data.Map as DM
import qualified Data.Set as DS

import qualified Relational.Class as R
import Relational.ColName
import Relational.Condition
import Relational.Naive (Relation, RelationalMonad, evalPure)
import ConditionGen (satisfying)

prop_makeSigAndTuples :: (Ord d) => ([ColName] -> [[d]] -> RelationalMonad d (Relation d)) -> ([ColName], [[d]]) -> Bool
prop_makeSigAndTuples mk = noErr . propM_makeSigAndTuples mk

propM_makeSigAndTuples :: (R.MonadRelational d r m) =>
                          ([ColName] -> [[d]] -> m r) -> ([ColName],[[d]]) -> m Bool
propM_makeSigAndTuples mk (names, tuples) =
    do r <- mk names tuples
       names' <- R.signature r
       tuples' <- R.tuples r
       return (toSetOfMaps names tuples == toSetOfMaps names' tuples')
    where toSetOfMaps names tuples =
              DS.fromList (map (DM.fromList . zip names) tuples)



prop_canRenameExistingToItself :: (Ord d) => Relation d -> Bool
prop_canRenameExistingToItself = noErr . propM_canRenameExistingToItself

propM_canRenameExistingToItself :: (R.MonadRelational d r m, Eq r) => r -> m Bool
propM_canRenameExistingToItself r =
    do s <- R.signature r
       (if null s
        then return True
        else let n = head s
             in (r==) `liftM` R.rename n n r)

prop_renameIsReversible :: (Ord d) => (Relation d, ColName) -> Bool
prop_renameIsReversible = noErr . propM_renameIsReversible

propM_renameIsReversible :: (R.MonadRelational d r m, Eq r) => (r, ColName) -> m Bool
propM_renameIsReversible (r, n) =
    do s <- R.signature r
       (if null s || n `elem` s
        then return True
        else let n' = head s
             in do r' <- R.rename n' n r
                   r'' <- R.rename n n' r'
                   return (r == r''))

prop_canRemoveIntermediateRenames :: (Ord d) => (Relation d, ColName, ColName) -> Bool
prop_canRemoveIntermediateRenames = noErr . propM_canRemoveIntermediateRenames

propM_canRemoveIntermediateRenames :: (R.MonadRelational d r m, Eq r) =>
                                      (r, ColName, ColName) -> m Bool
propM_canRemoveIntermediateRenames (r, n, m) =
    do s <- R.signature r
       (if null s || n `elem` s || m `elem` s
        then return True
        else do r' <- R.rename (head s) n r
                r'' <- R.rename n m r'
                r''' <- R.rename (head s) m r
                return (r'' == r'''))
               

prop_unionWithSelfIsSelf :: (Ord d) => Relation d -> Bool
prop_unionWithSelfIsSelf = noErr . propM_unionWithSelfIsSelf

propM_unionWithSelfIsSelf :: (R.MonadRelational d r m, Eq r) =>
                             r -> m Bool
propM_unionWithSelfIsSelf r =
    (r==) `liftM` R.union r r

prop_unionWithEmptyIsSelf :: (Ord d) => Relation d -> Bool
prop_unionWithEmptyIsSelf = noErr . propM_unionWithEmptyIsSelf

propM_unionWithEmptyIsSelf :: (R.MonadRelational d r m, Eq r) =>
                              r -> m Bool
propM_unionWithEmptyIsSelf r =
    (r==) `liftM` (R.union r =<< emptyLike r)

prop_unionIsCommutative :: (Ord d) => (Relation d, Relation d) -> Bool
prop_unionIsCommutative = noErr . propM_unionIsCommutative

propM_unionIsCommutative :: (R.MonadRelational d r m, Eq r) =>
                            (r, r) -> m Bool
propM_unionIsCommutative = propM_commutative R.union

prop_unionIsAssociative :: (Ord d) => (Relation d, Relation d, Relation d) -> Bool
prop_unionIsAssociative = noErr . propM_unionIsAssociative

propM_unionIsAssociative :: (R.MonadRelational d r m, Eq r) =>
                            (r, r, r) -> m Bool
propM_unionIsAssociative = propM_associative R.union

prop_unionLikeSetUnion :: (Ord d) => (Relation d, Relation d) -> Bool
prop_unionLikeSetUnion = noErr . propM_unionLikeSetUnion

propM_unionLikeSetUnion :: (R.MonadRelational d r m) => (r, r) -> m Bool
propM_unionLikeSetUnion = propM_likeSetOp R.union DS.union

prop_differenceWithSelfIsEmpty :: (Ord d) => Relation d -> Bool
prop_differenceWithSelfIsEmpty = noErr . propM_differenceWithSelfIsEmpty

propM_differenceWithSelfIsEmpty :: (R.MonadRelational d r m, Eq r) => r -> m Bool
propM_differenceWithSelfIsEmpty r =
    eqM (R.difference r r) (emptyLike r)

prop_differenceWithEmptyIsSelf :: (Ord d) => Relation d -> Bool
prop_differenceWithEmptyIsSelf = noErr . propM_differenceWithEmptyIsSelf

propM_differenceWithEmptyIsSelf :: (R.MonadRelational d r m, Eq r) => r -> m Bool
propM_differenceWithEmptyIsSelf r =
    (r==) `liftM` (R.difference r =<< emptyLike r)

prop_differenceDeMorgan1 :: (Ord d) => (Relation d, Relation d, Relation d) -> Bool
prop_differenceDeMorgan1 = noErr . propM_differenceDeMorgan1

propM_differenceDeMorgan1 :: (R.MonadRelational d r m, Eq r) => (r, r, r) -> m Bool
propM_differenceDeMorgan1 (r, s, t) =
    eqM (R.difference r =<< R.intersection s t)
            (join (liftM2 R.union (R.difference r s) (R.difference r t)))


prop_differenceEmptyDiffAnyIsEmpty :: (Ord d) => Relation d -> Bool
prop_differenceEmptyDiffAnyIsEmpty = noErr . propM_differenceEmptyDiffAnyIsEmpty

propM_differenceEmptyDiffAnyIsEmpty :: (R.MonadRelational d r m, Eq r) => r -> m Bool
propM_differenceEmptyDiffAnyIsEmpty r =
    eqM (emptyLike r) (flip R.difference r =<< (emptyLike r))

prop_differenceDeMorgan2 :: (Ord d) => (Relation d, Relation d, Relation d) -> Bool
prop_differenceDeMorgan2 = noErr . propM_differenceDeMorgan1

propM_differenceDeMorgan2 :: (R.MonadRelational d r m, Eq r) => (r, r, r) -> m Bool
propM_differenceDeMorgan2 (r, s, t) =
    eqM (R.difference r =<< R.union s t)
            (join (liftM2 R.intersection (R.difference r s) (R.difference r t)))

-- the next "differencePM" properties are from the properties of set
-- difference as listed on PlanetMath at
-- http://planetmath.org/encyclopedia/PropertiesOfSetDifference.html

prop_differencePM7a :: (Ord d) => (Relation d, Relation d) -> Bool
prop_differencePM7a = noErr . propM_differencePM7a

-- | property 7a: A - (A & B) = A - B
propM_differencePM7a :: (R.MonadRelational d r m, Eq r) => (r, r) -> m Bool
propM_differencePM7a (r, s) =
    eqM (R.difference r =<< (R.intersection r s)) (R.difference r s)

prop_differencePM7b :: (Ord d) => (Relation d, Relation d) -> Bool
prop_differencePM7b = noErr . propM_differencePM7b

-- | property 7b: (A + B) - B = A - B
propM_differencePM7b :: (R.MonadRelational d r m, Eq r) => (r, r) -> m Bool
propM_differencePM7b (r, s) =
    eqM (flip R.difference s =<< (R.union r s)) (R.difference r s)

prop_differencePM8 :: (Ord d) => (Relation d, Relation d, Relation d) -> Bool
prop_differencePM8 = noErr . propM_differencePM8

-- | property 8: (A & B) - C = (A - C) & (B - C)
propM_differencePM8 :: (R.MonadRelational d r m, Eq r) => (r, r, r) -> m Bool
propM_differencePM8 (r, s, t) =
    eqM (flip R.difference t =<< (R.intersection r s))
            (join $ liftM2 R.intersection  (R.difference r t) (R.difference s t))

prop_differencePM9 :: (Ord d) => (Relation d, Relation d, Relation d) -> Bool
prop_differencePM9 = noErr . propM_differencePM9

-- | property 9: A & (B - C) = (A & B) - (A & C)
propM_differencePM9 :: (R.MonadRelational d r m, Eq r) => (r, r, r) -> m Bool
propM_differencePM9 (r, s, t) =
    eqM (R.intersection r =<< R.difference s t)
            (join $ liftM2 R.difference (R.intersection r s) (R.intersection s t))

prop_differencePM10 :: (Ord d) => (Relation d, Relation d, Relation d, Relation d) -> Bool
prop_differencePM10 = noErr . propM_differencePM10

-- | property 10: (A - B) & (C - D) = (C - B) & (A - D)
propM_differencePM10 :: (R.MonadRelational d r m, Eq r) => (r, r, r, r) -> m Bool
propM_differencePM10 (r, s, t, u) =
    eqM (join $ liftM2 R.intersection (R.difference r s) (R.difference t u))
            (join $ liftM2 R.intersection (R.difference t s) (R.difference r u))

prop_differencePM11 :: (Ord d) => (Relation d, Relation d, Relation d, Relation d) -> Bool
prop_differencePM11 = noErr . propM_differencePM11

-- | property 11: (A - B) & (C - D) = (A & B) - (B + D)
propM_differencePM11 :: (R.MonadRelational d r m, Eq r) => (r, r, r, r) -> m Bool
propM_differencePM11 (r, s, t, u) =
    eqM (join $ liftM2 R.intersection (R.difference r s) (R.difference t u))
            (join $ liftM2 R.difference (R.intersection r t) (R.union s u))

prop_differenceLikeSetDifference :: (Ord d) => (Relation d, Relation d) -> Bool
prop_differenceLikeSetDifference = noErr . propM_differenceLikeSetDifference

propM_differenceLikeSetDifference :: (R.MonadRelational d r m) => (r, r) -> m Bool
propM_differenceLikeSetDifference = propM_likeSetOp R.difference DS.difference

prop_intersectionWithSelfIsSelf :: (Ord d) => Relation d -> Bool
prop_intersectionWithSelfIsSelf = noErr . propM_intersectionWithSelfIsSelf

propM_intersectionWithSelfIsSelf :: (R.MonadRelational d r m, Eq r) => r -> m Bool
propM_intersectionWithSelfIsSelf r = (r==) `liftM` R.intersection r r

prop_intersectionWithEmptyIsEmpty :: (Ord d) => Relation d -> Bool
prop_intersectionWithEmptyIsEmpty = noErr . propM_intersectionWithEmptyIsEmpty

propM_intersectionWithEmptyIsEmpty :: (R.MonadRelational d r m, Eq r) => r -> m Bool
propM_intersectionWithEmptyIsEmpty r = emptyLike r >>= \e -> (e==) `liftM` R.intersection r e

prop_intersectionIsCommutative :: (Ord d) => (Relation d, Relation d) -> Bool
prop_intersectionIsCommutative = noErr . propM_intersectionIsCommutative

propM_intersectionIsCommutative :: (R.MonadRelational d r m, Eq r) => (r, r) -> m Bool
propM_intersectionIsCommutative = propM_commutative R.intersection

prop_intersectionIsAssociative :: (Ord d) => (Relation d, Relation d, Relation d) -> Bool
prop_intersectionIsAssociative = noErr . propM_intersectionIsAssociative

propM_intersectionIsAssociative :: (R.MonadRelational d r m, Eq r) => (r, r, r) -> m Bool
propM_intersectionIsAssociative = propM_associative R.intersection

prop_intersectionDistributesOverUnion :: (Ord d) => (Relation d, Relation d, Relation d) -> Bool
prop_intersectionDistributesOverUnion = noErr . propM_intersectionDistributesOverUnion

propM_intersectionDistributesOverUnion :: (R.MonadRelational d r m, Eq r) => (r, r, r) -> m Bool
propM_intersectionDistributesOverUnion (r, s, t) =
    eqM (R.intersection r =<< R.union s t)
            (join (liftM2 R.union (R.intersection r s) (R.intersection r t)))

prop_intersectionLikeSetIntersection :: (Ord d) => (Relation d, Relation d) -> Bool
prop_intersectionLikeSetIntersection = noErr . propM_intersectionLikeSetIntersection

propM_intersectionLikeSetIntersection :: (R.MonadRelational d r m) => (r, r) -> m Bool
propM_intersectionLikeSetIntersection = propM_likeSetOp R.intersection DS.intersection

prop_exclusionsCommute :: (Ord d) => (Relation d, ColName, ColName) -> Bool
prop_exclusionsCommute = noErr . propM_exclusionsCommute

propM_exclusionsCommute :: (R.MonadRelational d r m, Eq r) => (r, ColName, ColName) -> m Bool
propM_exclusionsCommute (r, n1, n2) =
    eqM (exclude n2 =<< exclude n1 r) (exclude n1 =<< exclude n2 r)
    where exclude n r =
              do s <- R.signature r
                 R.project (delete n s) r

prop_projectionCommutesWithUnion :: (Ord d) => ((Relation d, Relation d), [ColName]) -> Bool
prop_projectionCommutesWithUnion = noErr. propM_projectionCommutesWithUnion

propM_projectionCommutesWithUnion :: (R.MonadRelational d r m, Eq r) =>
                                     ((r, r), [ColName]) -> m Bool
propM_projectionCommutesWithUnion = propM_projectionCommutesWith R.union

propM_projectionCommutesWith :: (R.MonadRelational d r m, Eq r) =>
                                (r -> r -> m r) -> ((r, r), [ColName]) -> m Bool
propM_projectionCommutesWith f (pair, names) =
    propM_commutesWith (R.project names) f pair

prop_projectionLikeMapProjection :: (Ord d) => (Relation d, [ColName]) -> Bool
prop_projectionLikeMapProjection = noErr . propM_projectionLikeMapProjection

propM_projectionLikeMapProjection :: (R.MonadRelational d r m, Eq r) =>
                                     (r, [ColName]) -> m Bool
propM_projectionLikeMapProjection (r, names) =
    do allNames <- R.signature r
       tuples <- R.tuples r
       eqM (R.make names (subTuples allNames tuples)) (R.project names r)
    where subTuples allNames = map (subTuple names)
              where subTuple names t = project names (DM.fromList (zip allNames t))
                    project names m = map (m DM.!) names

prop_selectTrueIsIdentity :: (Ord d) => Relation d -> Bool
prop_selectTrueIsIdentity = noErr . propM_selectTrueIsIdentity

propM_selectTrueIsIdentity :: (R.MonadRelational d r m, Eq r) =>
                              r -> m Bool
propM_selectTrueIsIdentity r = (r ==) `liftM` R.select CondTrue r

prop_selectFalseIsEmpty :: (Ord d) => Relation d -> Bool
prop_selectFalseIsEmpty = noErr . propM_selectFalseIsEmpty

propM_selectFalseIsEmpty :: (R.MonadRelational d r m, Eq r) =>
                            r -> m Bool
propM_selectFalseIsEmpty r = null `liftM` (R.tuples =<< R.select CondFalse r)

prop_selectUnsatisfiableIsEmpty :: (Ord d) =>
                                   (Relation d, Condition d (RelationalMonad d)) -> Bool
prop_selectUnsatisfiableIsEmpty = noErr . propM_selectUnsatisfiableIsEmpty

propM_selectUnsatisfiableIsEmpty :: (R.MonadRelational d r m) =>
                                    (r, Condition d m) -> m Bool
propM_selectUnsatisfiableIsEmpty (r, c) =
  null `liftM` (R.tuples =<< R.select c r)
  
prop_selectLikeFilter :: (Ord d) => (Relation d, Condition d (RelationalMonad d)) -> Bool
prop_selectLikeFilter = noErr . propM_selectLikeFilter

propM_selectLikeFilter :: (R.MonadRelational d r m, Eq r) =>
                          (r, Condition d m) -> m Bool
propM_selectLikeFilter (r, c) =
  do names <- R.signature r
     eqM (R.select c r) (fromMapList names =<< satisfying c =<< (toMapList names `liftM` R.tuples r))
  where toMapList names = map (DM.fromList . zip names)
        fromMapList names = R.make names . map (\m -> catMaybes $ map (flip DM.lookup m) names)

prop_prodWithNoAttrsIsId :: (Ord d) => Relation d -> Bool
prop_prodWithNoAttrsIsId = noErr . propM_prodWithNoAttrsIsId

propM_prodWithNoAttrsIsId :: (R.MonadRelational d r m, Eq r) =>
                             r -> m Bool
propM_prodWithNoAttrsIsId r =
  (r ==) `liftM` (R.cartesianProduct r =<< (R.make [] ([[]] :: [[d]])))
  
prop_prodWithEmptyIsEmpty :: (Ord d) => (Relation d, [ColName]) -> Bool
prop_prodWithEmptyIsEmpty = noErr . propM_prodWithEmptyIsEmpty

-- | Cartesian product with an empty relation with distinct attributes
-- always produces the empty relation.
propM_prodWithEmptyIsEmpty :: (R.MonadRelational d r m) =>
                              (r, [ColName]) -> m Bool
propM_prodWithEmptyIsEmpty (r, attrs) =
  null `liftM` ( R.tuples =<< R.cartesianProduct r =<< R.make attrs ([] :: [[d]]))

prop_prodLikeConcat :: (Ord d) => (Relation d, Relation d) -> Bool
prop_prodLikeConcat = noErr . propM_prodLikeConcat

-- | Cartesian product is like taking all concatenations of tuple
-- lists. Note that the input relations should be product-compatible.
propM_prodLikeConcat :: (R.MonadRelational d r m, Eq r) =>
                        (r, r) -> m Bool
propM_prodLikeConcat (r, s) =
  eqM (allListConcats r s) (R.cartesianProduct r s)
  where allListConcats r s =
          do sigr <- R.signature r
             sigs <- R.signature s
             tr <- R.tuples r
             ts <- R.tuples s
             R.make (sigr ++ sigs) [x ++ y | x <- tr, y <- ts]

prop_joinLikeSelectOnProd :: (Ord d) => (Relation d, Relation d, Condition d (RelationalMonad d)) -> Bool
prop_joinLikeSelectOnProd = noErr . propM_joinLikeSelectOnProd

-- | Joining two product-compatible relations is the same as
-- performing a select on their Cartesian product.
propM_joinLikeSelectOnProd :: (R.MonadRelational d r m, Eq r) =>
                              (r, r, Condition d m) -> m Bool
propM_joinLikeSelectOnProd (r, s, c) =
  eqM (R.select c =<< R.cartesianProduct r s) (R.join c r s)

emptyLike :: (R.MonadRelational d r m) => r -> m r
emptyLike r = R.signature r >>= flip R.make []

tupleSet :: (R.MonadRelational d r m) => r -> m (DS.Set [d])
tupleSet = liftM DS.fromList . R.tuples

propM_likeSetOp :: (R.MonadRelational d r m) =>
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

propM_commutesWith :: (Monad m, Eq a) => (a -> m a) -> (a -> a -> m a) -> (a, a) -> m Bool
propM_commutesWith f g (x,y) =
    eqM (f =<< g x y) (join (liftM2 g (f x) (f y)))

noErr :: RelationalMonad d Bool -> Bool
noErr = either (const False) id . evalPure

eqM :: (Monad m, Eq a) => m a -> m a -> m Bool
eqM = liftM2 (==)
