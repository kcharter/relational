{-# LANGUAGE TupleSections #-}

module RelationTests (run) where

import Control.Monad (liftM2)
import Test.QuickCheck

import qualified Relational.Class as R
import Relational.ColName
import Relational.Condition
import Relational.Naive (Relation, RelationalMonad)
import qualified Relational.Naive.Signature as Sig

import ColNameGen()
import ConditionGen
import RelationGen
import RelationalProps
import SignatureGen (nonEmptySignatures, disjointSignatures)
import SubList

run :: IO ()
run = do quickCheck $ forAll (inputs 5) (prop_makeSigAndTuples makeIntRelation :: ([ColName], [[Int]]) -> Bool)
         quickCheck (prop_canRenameExistingToItself :: RInt -> Bool)
         quickCheck (prop_renameIsReversible :: (RInt, ColName) -> Bool)
         quickCheck (prop_canRemoveIntermediateRenames :: (RInt, ColName, ColName) -> Bool)
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
         quickCheck $ forAllRIntAndTwoAttrs (prop_exclusionsCommute :: (RInt, ColName, ColName) -> Bool)
         quickCheck $ forAllUC2AndAttrs (prop_projectionCommutesWithUnion :: (RInt2, [ColName]) -> Bool)
         quickCheck $ forAllRIntAndAttrs (prop_projectionLikeMapProjection :: (RInt, [ColName]) -> Bool)
         quickCheck (prop_selectTrueIsIdentity :: RInt -> Bool)
         quickCheck (prop_selectFalseIsEmpty :: RInt -> Bool)
         quickCheck $ forAllRSmallAndUnsatCond (prop_selectUnsatisfiableIsEmpty :: (RSmall, CondSmall) -> Bool)
         quickCheck $ forAllRSmallAndSatCond (prop_selectLikeFilter :: (RSmall, CondSmall) -> Bool)
         quickCheck (prop_prodWithNoAttrsIsId :: RSmall -> Bool)
         quickCheck $ forAll rSmallAndDistinctAttrs (prop_prodWithEmptyIsEmpty :: (RSmall, [ColName]) -> Bool)
         quickCheck $ forAllPCPair (prop_prodLikeConcat :: (RSmall, RSmall) -> Bool)
         quickCheck $ forAllPCPairAndSatCond (prop_joinLikeSelectOnProd :: (RBool, RBool, CondBool) -> Bool)

type RInt = Relation Int
type RInt2 = (RInt, RInt)
type RInt3 = (RInt, RInt, RInt)
type RInt4 = (RInt, RInt, RInt, RInt)

makeIntRelation :: [ColName] -> [[Int]] -> RelationalMonad Int RInt
makeIntRelation = makeRelation

makeRelation :: (Ord a) => [ColName] -> [[a]] -> RelationalMonad a (Relation a)
makeRelation = R.make


forAllUC2 = forAll unionCompatiblePair
forAllUC3 = forAll unionCompatibleTriple
forAllUC4 = forAll unionCompatibleFour

forAllRIntAndTwoAttrs = forAll (relationAndTwoAttrs :: Gen (RInt, ColName, ColName))

forAllRIntAndAttrs = forAll (do n <- choose (0,6); m <- choose (0,n); relationAndAttrs n m)

forAllUC2AndAttrs = forAll unionCompatiblePairAndAttrs

type RSmall = Relation Small
type CondSmall = Condition Small (RelationalMonad Small)

forAllRSmallAndSatCond = forAll rSmallAndSatCond

rSmallAndSatCond :: Gen (RSmall, CondSmall) 
rSmallAndSatCond =
  sized $ \n -> do sig <- nonEmptySignatures 3
                   (c, allSat) <- satisfiableCondition (Sig.toList sig) n
                   someSat <- subList allSat =<< choose (0,length allSat)
                   maybeSat <- listOf $ vectorOf (Sig.size sig) arbitrary
                   return (makeOrDie sig (someSat ++ maybeSat), c)
                   
forAllRSmallAndUnsatCond = forAll rSmallAndUnsatCond

rSmallAndUnsatCond :: Gen (RSmall, CondSmall)
rSmallAndUnsatCond =
  sized $ \n -> do sig <- nonEmptySignatures 3
                   c <- unsatisfiableCondition (Sig.toList sig) n
                   r <- relationWithSig sig
                   return (r, c)

rSmallAndDistinctAttrs :: Gen (RSmall, [ColName])
rSmallAndDistinctAttrs =
  disjointSignatures arbitrary >>= \(s1,s2) -> liftM2 (,) (relationWithSig s1) (return (Sig.toList s2))
     
type RBool = Relation Bool
type CondBool = Condition Bool (RelationalMonad Bool)

forAllPCPair = forAll productCompatiblePair

forAllPCPairAndSatCond = forAll productCompatiblePairAndSatisfiableCondition
