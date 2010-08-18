module RelationalProps where

import Control.Monad (liftM)
import qualified Relational.Class as R

prop_canRenameExistingToItself :: (R.Relational n d r, Eq r) => r -> Bool
prop_canRenameExistingToItself r =
    noErr $ do s <- R.signature r
               (if null s
                then return True
                else let n = head s
                     in (r==) `liftM` R.rename n n r)

prop_renameIsReversible :: (R.Relational n d r, Eq r) => (r, n) -> Bool
prop_renameIsReversible (r, n) =
    noErr $ do s <- R.signature r
               (if null s || n `elem` s
                then return True
                else let n' = head s
                     in do r' <- R.rename n' n r
                           r'' <- R.rename n n' r'
                           return (r' == r''))

prop_canRemoveIntermediateRenames :: (R.Relational n d r, Eq r) => (r, n, n) -> Bool
prop_canRemoveIntermediateRenames (r, n, m) =
    noErr $ do s <- R.signature r
               (if null s || n `elem` s || m `elem` s
                then return True
                else do r' <- R.rename (head s) n r
                        r'' <- R.rename n m r'
                        r''' <- R.rename (head s) m r
                        return (r' == r'''))
               

noErr :: Either String b -> Bool
noErr = either (const False) (const True)
    