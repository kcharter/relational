{-| Generators for relation signatures. -}

module SignatureGen where

import Control.Monad (liftM)
import Test.QuickCheck

import qualified Relational.Naive.Signature as Sig

import Relational.ColName
import ColNameGen()
import MonadUtil

instance Arbitrary Sig.Signature where
    arbitrary = Sig.fromList `liftM` (arbitrary :: Gen [ColName])
    shrink s = map Sig.fromList (shrink (Sig.toList s :: [ColName]))


signatures :: Int -> Gen Sig.Signature
signatures maxSize =
    Sig.fromList `liftM` (resize maxSize arbitrary :: Gen [ColName])

nonEmptySignatures :: Int -> Gen Sig.Signature
nonEmptySignatures maxSize = untilM ((0<) . Sig.size) (signatures maxSize)
  
signaturesOfSize :: Int -> Gen Sig.Signature
signaturesOfSize size =
    untilM ((size ==) . Sig.size) (Sig.fromList `liftM` (vectorOf size arbitrary :: Gen [ColName]))

disjointSignatures :: Gen Sig.Signature -> Gen (Sig.Signature, Sig.Signature)
disjointSignatures g =
    do s1 <- g
       s2 <- untilM (Sig.disjoint s1) g
       return (s1,s2)

signatureAndNewName :: Gen Sig.Signature -> Gen (Sig.Signature, ColName)
signatureAndNewName g =
    do s <- g
       n <- untilM (not . flip Sig.contains s) arbitrary
       return (s, n)

