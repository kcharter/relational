{-| Generators for relation signatures. -}

module SignatureGen where

import Control.Monad (liftM)
import Test.QuickCheck

import qualified Relational.Naive.Signature as Sig

import Relational.Naive.AttrName
import AttrNameGen

instance Arbitrary Sig.Signature where
    arbitrary = Sig.fromList `liftM` (arbitrary :: Gen [AttrName])
    shrink s = map Sig.fromList (shrink (Sig.toList s :: [AttrName]))


signatures :: Int -> Gen Sig.Signature
signatures maxSize =
    Sig.fromList `liftM` (resize maxSize arbitrary :: Gen [AttrName])

disjointSignatures :: Gen Sig.Signature -> Gen (Sig.Signature, Sig.Signature)
disjointSignatures g =
    do s1 <- g
       s2 <- untilM (Sig.disjoint s1) g
       return (s1,s2)

signatureAndNewName :: Gen Sig.Signature -> Gen (Sig.Signature, AttrName)
signatureAndNewName g =
    do s <- g
       n <- untilM (not . flip Sig.contains s) arbitrary
       return (s, n)

untilM :: (Monad m) => (a -> Bool) -> m a -> m a
untilM test m =
    do x <- m
       if test x then return x else untilM test m