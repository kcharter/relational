module RelationTests (run) where

import Test.QuickCheck

--import qualified Relational.Class as R
import Relational.Naive.AttrName
import Relational.Naive (Relation)

import AttrNameGen()
import SignatureGen()
import RelationGen()
import RelationalProps

run :: IO ()
run = do quickCheck (prop_canRenameExistingToItself :: Relation Int -> Bool)
         quickCheck (prop_renameIsReversible :: (Relation Int, AttrName) -> Bool)
         quickCheck (prop_canRemoveIntermediateRenames :: (Relation Int, AttrName, AttrName) -> Bool)


