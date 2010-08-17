module RelationGen where

import Control.Monad (liftM, replicateM, join)
import Test.QuickCheck

import Relational.Naive.AttrName
import qualified Relational.Naive.Signature as Sig
import qualified Relational.Class as C
import qualified Relational.Naive as RN

import SignatureGen

instance (Ord a, Arbitrary a) => Arbitrary (RN.Relation a) where
    arbitrary = join (relationOfSize arbitrary `liftM` choose (0,6))

-- | Generates a relation over a type of generated data, with a given
-- signature and a desired number of tuples. The desired size is a
-- suggested approximate size only, since the nature of the value
-- generator and the signature may make it impossible to guarantee the
-- size. For example, if the signature is empty, the only possible
-- sizes are zero and one.
relationWithSig :: (Ord a) => Gen a -> Sig.Signature -> Int -> Gen (RN.Relation a)
relationWithSig g sig size =
    make names `liftM` tuples
    where make names = either failure id . C.make names
          failure msg = error ("Failed to generate a relation with signature " ++
                               show sig ++ " and size " ++ show size ++ ": " ++
                               msg)
          names = Sig.toList sig :: [AttrName]
          tuples = replicateM size tuple 
          tuple = vectorOf (Sig.size sig) g

relationOfSize :: (Ord a) => Gen a -> Int -> Gen (RN.Relation a)
relationOfSize g size =
    join (flip (relationWithSig g) size `liftM` arbitrary)