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
         quickCheck $ forAllUC2 (prop_unionIsCommutative :: RInt2 -> Bool)
         quickCheck $ forAllUC3 (prop_unionIsAssociative :: RInt3 -> Bool)
         quickCheck $ forAllUC2 (prop_unionLikeSetUnion :: RInt2 -> Bool)
         quickCheck (prop_differenceWithSelfIsEmpty :: RInt -> Bool)
         quickCheck (prop_differenceWithEmptyIsSelf :: RInt -> Bool)
         quickCheck (prop_differenceEmptyDiffAnyIsEmpty :: RInt -> Bool)
         quickCheck $ forAllUC3 (prop_differenceDeMorgan1 :: RInt3 -> Bool)
         quickCheck $ forAllUC3 (prop_differenceDeMorgan2 :: RInt3 -> Bool)
         quickCheck $ forAllUC2 (prop_differencePM7a :: RInt2 -> Bool)
         quickCheck $ forAllUC2 (prop_differencePM7b :: RInt2 -> Bool)
         quickCheck $ forAllUC3 (prop_differencePM8 :: RInt3 -> Bool)
         quickCheck $ forAllUC3 (prop_differencePM9 :: RInt3 -> Bool)
         quickCheck $ forAllUC4 (prop_differencePM10 :: RInt4 -> Bool)
         quickCheck $ forAllUC4 (prop_differencePM11 :: RInt4 -> Bool)
         quickCheck $ forAllUC2 (prop_differenceLikeSetDifference :: RInt2 -> Bool)
         quickCheck (prop_intersectionWithSelfIsSelf :: RInt -> Bool)
         quickCheck (prop_intersectionWithEmptyIsEmpty :: RInt -> Bool)
         quickCheck $ forAllUC2 (prop_intersectionIsCommutative :: RInt2 -> Bool)
         quickCheck $ forAllUC3 (prop_intersectionIsAssociative :: RInt3 -> Bool)
         quickCheck $ forAllUC3 (prop_intersectionDistributesOverUnion :: RInt3 -> Bool)
         quickCheck $ forAllUC2 (prop_intersectionLikeSetIntersection :: RInt2 -> Bool)
         quickCheck $ forAllRIntAndTwoAttrs (prop_exclusionsCommute :: (RInt, AttrName, AttrName) -> Bool)
         quickCheck $ forAllUC2AndAttrs (prop_projectionCommutesWithUnion :: (RInt2, [AttrName]) -> Bool)
         quickCheck $ forAllUC2AndAttrs (prop_projectionCommutesWithDifference :: (RInt2, [AttrName]) -> Bool)
         quickCheck $ forAllUC2AndAttrs (prop_projectionCommutesWithIntersection :: (RInt2, [AttrName]) -> Bool)
         quickCheck $ forAllRIntAndAttrs (prop_projectionLikeMapProjection :: (RInt, [AttrName]) -> Bool)

type RInt = Relation Int
type RInt2 = (RInt, RInt)
type RInt3 = (RInt, RInt, RInt)
type RInt4 = (RInt, RInt, RInt, RInt)

makeIntRelation :: [AttrName] -> [[Int]] -> Either String RInt
makeIntRelation = makeRelation

makeRelation :: (Ord a) => [AttrName] -> [[a]] -> Either String (Relation a)
makeRelation = R.make


forAllUC2 = forAll unionCompatiblePair
forAllUC3 = forAll unionCompatibleTriple
forAllUC4 = forAll unionCompatibleFour

forAllRIntAndTwoAttrs = forAll (relationAndTwoAttrs :: Gen (RInt, AttrName, AttrName))

forAllRIntAndAttrs = forAll (do n <- choose (0,6); m <- choose (0,n); relationAndAttrs n m)

forAllUC2AndAttrs = forAll unionCompatiblePairAndAttrs
