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
         quickCheck (prop_canRenameExistingToItself :: RInt -> Bool)
         quickCheck (prop_renameIsReversible :: (RInt, AttrName) -> Bool)
         quickCheck (prop_canRemoveIntermediateRenames :: (RInt, AttrName, AttrName) -> Bool)
         quickCheck (prop_unionWithSelfIsSelf :: RInt -> Bool)
         quickCheck (prop_unionWithEmptyIsSelf :: RInt -> Bool)
         quickCheck $ forAll unionCompatiblePair (prop_unionIsCommutative :: (RInt, RInt) -> Bool)
         quickCheck $ forAll unionCompatibleTriple (prop_unionIsAssociative :: (RInt, RInt, RInt) -> Bool)
         quickCheck $ forAll unionCompatiblePair (prop_unionLikeSetUnion :: (RInt, RInt) -> Bool)
         quickCheck (prop_differenceWithSelfIsEmpty :: RInt -> Bool)
         quickCheck (prop_differenceWithEmptyIsSelf :: RInt -> Bool)
         quickCheck (prop_differenceEmptyDiffAnyIsEmpty :: RInt -> Bool)
         quickCheck $ forAll unionCompatibleTriple (prop_differenceDeMorgan1 :: (RInt, RInt, RInt) -> Bool)
         quickCheck $ forAll unionCompatibleTriple (prop_differenceDeMorgan2 :: (RInt, RInt, RInt) -> Bool)
         quickCheck $ forAll unionCompatiblePair (prop_differencePM7a :: (RInt, RInt) -> Bool)
         quickCheck $ forAll unionCompatiblePair (prop_differencePM7b :: (RInt, RInt) -> Bool)
         quickCheck $ forAll unionCompatibleTriple (prop_differencePM8 :: (RInt, RInt, RInt) -> Bool)
         quickCheck $ forAll unionCompatibleTriple (prop_differencePM9 :: (RInt, RInt, RInt) -> Bool)
         quickCheck $ forAllUC4 (prop_differencePM10 :: (RInt, RInt, RInt, RInt) -> Bool)
         quickCheck $ forAllUC4 (prop_differencePM11 :: (RInt, RInt, RInt, RInt) -> Bool)
         quickCheck $ forAll unionCompatiblePair (prop_differenceLikeSetDifference :: (RInt, RInt) -> Bool)
         quickCheck (prop_intersectionWithSelfIsSelf :: RInt -> Bool)
         quickCheck (prop_intersectionWithEmptyIsEmpty :: RInt -> Bool)
         quickCheck $ forAll unionCompatiblePair (prop_intersectionIsCommutative :: (RInt, RInt) -> Bool)
         quickCheck $ forAll unionCompatibleTriple (prop_intersectionIsAssociative :: (RInt, RInt, RInt) -> Bool)
         quickCheck $ forAll unionCompatibleTriple (prop_intersectionDistributesOverUnion :: (RInt, RInt, RInt) -> Bool)
         quickCheck $ forAll unionCompatiblePair (prop_intersectionLikeSetIntersection :: (RInt, RInt) -> Bool)

type RInt = Relation Int

makeIntRelation :: [AttrName] -> [[Int]] -> Either String RInt
makeIntRelation = makeRelation

makeRelation :: (Ord a) => [AttrName] -> [[a]] -> Either String (Relation a)
makeRelation = R.make


forAllUC4 = forAll unionCompatibleFour