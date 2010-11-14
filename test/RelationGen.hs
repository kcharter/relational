module RelationGen where

import Control.Monad (liftM, liftM2, liftM3, liftM4, join)
import Test.QuickCheck

import Relational.Naive.AttrName
import Relational.Condition (Condition)
import qualified Relational.Naive.Signature as Sig
import qualified Relational.Class as C
import qualified Relational.Naive as RN

import SignatureGen
import ConditionGen
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

unionCompatiblePairAndAttrs :: (Ord a, Arbitrary a) => Gen ((RN.Relation a, RN.Relation a), [AttrName])
unionCompatiblePairAndAttrs =
    do (x,y) <- unionCompatiblePair
       allNames <- C.signature x
       n <- choose (0, length allNames)
       names <- subList allNames n
       return ((x,y), names)

-- | A generator for pairs of relations with disjoint signatures, and
-- hence suitable for Cartesian products and joins.
productCompatiblePair :: (Ord a, Arbitrary a) => Gen (RN.Relation a, RN.Relation a)
productCompatiblePair =
  do (s1,s2) <- disjointSignatures (signatures 4)
     liftM2 (,) (relationWithSig s1) (relationWithSig s2)

-- | Generates pairs of product-compatible relations and a satisfiable
-- condition on the union of their signatures. Typically, the
-- Cartesian product of the relations will contain some random number
-- of the tuples that satisfy the condition.
productCompatiblePairAndSatisfiableCondition :: (Bounded a, Enum a, Ord a, Show a, Arbitrary a, CoArbitrary a) =>
                                                Gen (RN.Relation a, RN.Relation a,
                                                     Condition AttrName a (Either String))
productCompatiblePairAndSatisfiableCondition =
  do (s1,s2) <- disjointSignatures (signatures 3)
     (c,ts)  <- satisfiableCondition (Sig.toList s1 ++ Sig.toList s2) =<< choose (1,8)
     n <- sized $ \n -> choose (1, min n (length ts))
     pairs <- map (splitAt (Sig.size s1)) `liftM` subList ts n
     r   <- mk s1 (map fst pairs)
     s   <- mk s2 (map snd pairs)
     return (r,s,c)
  where mk sig common = (makeOrDie sig . (common++)) `liftM` tuples'
          where tuples' = sized $ \n -> resize (min 0 (n - length common)) (tuples (Sig.size sig))
