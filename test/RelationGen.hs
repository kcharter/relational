module RelationGen where

import Control.Monad (liftM, liftM2, liftM3, liftM4, join)
import Test.QuickCheck

import Relational.Naive.AttrName
import qualified Relational.Naive.Signature as Sig
import qualified Relational.Class as C
import qualified Relational.Naive as RN

import SignatureGen
import SubList (subList)

instance (Ord a, Arbitrary a) => Arbitrary (RN.Relation a) where
    arbitrary = join (relationOfSize arbitrary `liftM` choose (0,6))

inputs :: (Arbitrary a) => Int -> Gen ([AttrName], [[a]])
inputs maxAttrs =
    do names <- Sig.toList `liftM` signatures maxAttrs
       tuples <- tuples (length names)
       return (names, tuples)

tuples :: (Arbitrary a) => Int -> Gen [[a]]
tuples len = listOf (vectorOf len arbitrary)

unionCompatiblePair :: (Ord a, Arbitrary a) => Gen (RN.Relation a, RN.Relation a)
unionCompatiblePair =
    arbitrary >>= \s -> let g = relationWithSig s in liftM2 (,) g g

unionCompatibleTriple :: (Ord a, Arbitrary a) => Gen (RN.Relation a, RN.Relation a, RN.Relation a)
unionCompatibleTriple =
    arbitrary >>= \s -> let g = relationWithSig s in liftM3 (,,) g g g

unionCompatibleFour :: (Ord a, Arbitrary a) => Gen (RN.Relation a, RN.Relation a, RN.Relation a, RN.Relation a)
unionCompatibleFour =
    arbitrary >>= \s -> let g = relationWithSig s in liftM4 (,,,) g g g g

relationWithSig :: (Ord a, Arbitrary a) => Sig.Signature -> Gen (RN.Relation a)
relationWithSig s =
    makeOrDie s `liftM` tuples (Sig.size s)

-- | Generates a relation over a type of generated data, with a given
-- signature and a desired number of tuples. The desired size is a
-- suggested approximate size only, since the nature of the value
-- generator and the signature may make it impossible to guarantee the
-- size. For example, if the signature is empty, the only possible
-- sizes are zero and one.
relationWithSigAndSize :: (Ord a) => Gen a -> Sig.Signature -> Int -> Gen (RN.Relation a)
relationWithSigAndSize g sig size =
    makeOrDie sig `liftM` tuples
    where tuples = vectorOf size tuple 
          tuple = vectorOf (Sig.size sig) g

relationOfSize :: (Ord a) => Gen a -> Int -> Gen (RN.Relation a)
relationOfSize g size =
    join (flip (relationWithSigAndSize g) size `liftM` arbitrary)

makeOrDie :: (Ord a) => Sig.Signature -> [[a]] -> RN.Relation a
makeOrDie sig = either failure id . C.make names
    where failure msg = error ("Failed to generate a relation with signature " ++
                               show sig ++ ": " ++
                               msg)
          names = Sig.toList sig :: [AttrName]

relationAndTwoAttrs :: (Ord a, Arbitrary a) => Gen (RN.Relation a, AttrName, AttrName)
relationAndTwoAttrs =
    (\(r, a1:a2:_) -> (r,a1,a2)) `liftM` relationAndAttrs 4 2

-- | The generator @relationAndAttrs sigN n@ generates pairs
-- containing a relation of @sigN@ attributes, and a list of @n@
-- attribute names drawn from the signature. This is meant for tests
-- that exercise projection.
relationAndAttrs :: (Ord a, Arbitrary a) => Int -> Int -> Gen (RN.Relation a, [AttrName])
relationAndAttrs sigN n =
    do sig <- signaturesOfSize sigN
       tuples <- tuples sigN
       subSig <- subList (Sig.toList sig) n
       return (makeOrDie sig tuples, subSig)

