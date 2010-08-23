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
         quickCheck (prop_unionWithSelfIsSelf :: Relation Int -> Bool)
         quickCheck (prop_unionWithEmptyIsSelf :: Relation Int -> Bool)
         quickCheck $ forAll unionCompatiblePair (prop_unionIsCommutative :: (Relation Int, Relation Int) -> Bool)
         quickCheck $ forAll unionCompatibleTriple (prop_unionIsAssociative :: (Relation Int, Relation Int, Relation Int) -> Bool)
         quickCheck $ forAll unionCompatiblePair (prop_unionLikeSetUnion :: (Relation Int, Relation Int) -> Bool)
         quickCheck (prop_differenceWithSelfIsEmpty :: Relation Int -> Bool)
         quickCheck (prop_differenceWithEmptyIsSelf :: Relation Int -> Bool)
         quickCheck (prop_differenceEmptyDiffAnyIsEmpty :: Relation Int -> Bool)
         quickCheck $ forAll unionCompatibleTriple (prop_differenceDeMorgan1 :: (Relation Int, Relation Int, Relation Int) -> Bool)
         quickCheck $ forAll unionCompatibleTriple (prop_differenceDeMorgan2 :: (Relation Int, Relation Int, Relation Int) -> Bool)
         quickCheck $ forAll unionCompatiblePair (prop_differencePM7a :: (Relation Int, Relation Int) -> Bool)
         quickCheck $ forAll unionCompatiblePair (prop_differencePM7b :: (Relation Int, Relation Int) -> Bool)
         quickCheck $ forAll unionCompatibleTriple (prop_differencePM8 :: (Relation Int, Relation Int, Relation Int) -> Bool)
         quickCheck $ forAll unionCompatibleTriple (prop_differencePM9 :: (Relation Int, Relation Int, Relation Int) -> Bool)
         quickCheck $ forAllUC4 (prop_differencePM10 :: (Relation Int, Relation Int, Relation Int, Relation Int) -> Bool)
         quickCheck $ forAllUC4 (prop_differencePM11 :: (Relation Int, Relation Int, Relation Int, Relation Int) -> Bool)
         quickCheck $ forAll unionCompatiblePair (prop_differenceLikeSetDifference :: (Relation Int, Relation Int) -> Bool)
         quickCheck (prop_intersectionWithSelfIsSelf :: Relation Int -> Bool)
         quickCheck (prop_intersectionWithEmptyIsEmpty :: Relation Int -> Bool)
         quickCheck $ forAll unionCompatiblePair (prop_intersectionIsCommutative :: (Relation Int, Relation Int) -> Bool)
         quickCheck $ forAll unionCompatibleTriple (prop_intersectionIsAssociative :: (Relation Int, Relation Int, Relation Int) -> Bool)
         quickCheck $ forAll unionCompatibleTriple (prop_intersectionDistributesOverUnion :: (Relation Int, Relation Int, Relation Int) -> Bool)
         quickCheck $ forAll unionCompatiblePair (prop_intersectionLikeSetIntersection :: (Relation Int, Relation Int) -> Bool)

makeIntRelation :: [AttrName] -> [[Int]] -> Either String (Relation Int)
makeIntRelation = makeRelation

makeRelation :: (Ord a) => [AttrName] -> [[a]] -> Either String (Relation a)
makeRelation = R.make


forAllUC4 = forAll unionCompatibleFour