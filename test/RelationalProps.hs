module RelationalProps where

import Control.Monad (liftM, liftM2)
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
propM_unionLikeSetUnion (r, s) =
    liftM2 (==) (R.union r s >>= tupleSet) (liftM2 DS.union (tupleSet r) (tupleSet s))

prop_intersectionLikeSetIntersection :: (R.Relational n d r) => (r, r) -> Bool
prop_intersectionLikeSetIntersection = noErr . propM_intersectionLikeSetIntersection

propM_intersectionLikeSetIntersection :: (R.Relational n d r, Error e, MonadError e m) => (r, r) -> m Bool
propM_intersectionLikeSetIntersection (r, s) =
    liftM2 (==) (R.intersection r s >>= tupleSet) (liftM2 DS.intersection (tupleSet r) (tupleSet s))

emptyLike :: (R.Relational n d r, Error e, MonadError e m) => r -> m r
emptyLike r = R.signature r >>= flip R.make []

tupleSet :: (R.Relational n d r, Error e, MonadError e m) => r -> m (DS.Set [d])
tupleSet = liftM DS.fromList . R.tuples

propM_commutative :: (Monad m, Eq a) => (a -> a -> m a) -> (a, a) -> m Bool
propM_commutative f (x,y) = liftM2 (==) (f x y) (f y x)

propM_associative :: (Monad m, Eq a) => (a -> a -> m a) -> (a, a, a) -> m Bool
propM_associative f (x,y,z) =
    liftM2 (==) (f x y >>= flip f z) (f x =<< f y z)

noErr :: Either String b -> Bool
noErr = either (const False) (const True)
