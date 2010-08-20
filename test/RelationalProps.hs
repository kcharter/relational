module RelationalProps where

import Control.Monad (liftM)
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
               

noErr :: Either String b -> Bool
noErr = either (const False) (const True)
    