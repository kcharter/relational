module RelationTests (run) where

import Test.QuickCheck

import qualified Relational.Class as R
import Relational.Naive.AttrName
import Relational.Naive (Relation)

import AttrNameGen()
import SignatureGen()
import RelationGen
import RelationalProps

run :: IO ()
run = do quickCheck $ forAll (inputs 5) (prop_makeSigAndTuples makeIntRelation :: ([AttrName], [[Int]]) -> Bool)
         quickCheck (prop_canRenameExistingToItself :: Relation Int -> Bool)
         quickCheck (prop_renameIsReversible :: (Relation Int, AttrName) -> Bool)
         quickCheck (prop_canRemoveIntermediateRenames :: (Relation Int, AttrName, AttrName) -> Bool)

makeIntRelation :: [AttrName] -> [[Int]] -> Either String (Relation Int)
makeIntRelation = makeRelation

makeRelation :: (Ord a) => [AttrName] -> [[a]] -> Either String (Relation a)
makeRelation = R.make


