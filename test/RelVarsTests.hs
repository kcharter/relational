module RelVarsTests where

import Data.List (foldl', sort)
import Test.QuickCheck

import Relational.RelName (RelName)
import Relational.Naive.Relation (Relation)
import qualified Relational.Naive.RelVars as RV

import RelNameGen ()
import RelationGen ()
import RelVarsGen ()

run :: IO ()
run = do quickCheck prop_emptyToList
         quickCheck prop_emptyFromEmptyList
         quickCheck prop_emptyNeverGets
         quickCheck prop_emptyNeverContains
         quickCheck prop_emptyHasSizeZero
         quickCheck prop_sizeIsLenToList
         quickCheck prop_putAddsName
         quickCheck prop_putAddsBindingPair
         quickCheck prop_getPut
         quickCheck prop_relNamesAreAllContained
         quickCheck prop_relNamesAreFromBindings
         quickCheck prop_fromListToList
         quickCheck prop_fromListLikeIteratedPut
         quickCheck prop_modifyLikeReplacement
         quickCheck $ forAll (arbitrary `suchThat` (\(n,_,rv) -> not (RV.contains n rv))) prop_modifyOnAbsentName

prop_emptyToList :: Bool
prop_emptyToList = null (RV.toList RV.empty)

prop_emptyFromEmptyList :: Bool
prop_emptyFromEmptyList = RV.empty == (RV.fromList [] :: RV.RelVars Int)

prop_emptyNeverGets :: RelName -> Bool
prop_emptyNeverGets n =
  maybe True (const False) (RV.get n RV.empty)

prop_emptyNeverContains :: RelName -> Bool
prop_emptyNeverContains n =  not $ RV.contains n (RV.empty :: RV.RelVars Int)

prop_emptyHasSizeZero :: Bool
prop_emptyHasSizeZero = 0 == RV.size (RV.empty :: RV.RelVars Int)

prop_sizeIsLenToList :: RV.RelVars Int -> Bool
prop_sizeIsLenToList rv = RV.size rv == length (RV.toList rv) 

prop_putAddsName :: (RelName, Relation Int, RV.RelVars Int) -> Bool
prop_putAddsName (n, r, rv) =
  let rv' = RV.put n r rv
  in n `elem` RV.relNames rv'
     
prop_putAddsBindingPair :: (RelName, Relation Int, RV.RelVars Int) -> Bool
prop_putAddsBindingPair (n, r, rv) =
  (n,r) `elem` RV.toList (RV.put n r rv)

prop_getPut :: (RelName, Relation Int, RV.RelVars Int) -> Bool
prop_getPut (n,r,rv) =
  let rv' = RV.put n r rv
  in maybe False (==r) (RV.get n rv') 

prop_relNamesAreAllContained :: RV.RelVars Int -> Bool
prop_relNamesAreAllContained rv = all (flip RV.contains rv) (RV.relNames rv)

prop_relNamesAreFromBindings :: RV.RelVars Int -> Bool
prop_relNamesAreFromBindings rv = sort (RV.relNames rv) == sort (map fst (RV.toList rv))

prop_fromListToList :: RV.RelVars Int -> Bool
prop_fromListToList rv = rv == RV.fromList (RV.toList rv)

prop_fromListLikeIteratedPut :: RV.RelVars Int -> Bool
prop_fromListLikeIteratedPut rv =
  let pairs = RV.toList rv
  in RV.fromList pairs == foldl' (flip (uncurry RV.put)) RV.empty pairs
     
prop_modifyLikeReplacement :: (RelName, Relation Int, Relation Int, RV.RelVars Int) -> Bool
prop_modifyLikeReplacement (n, x, y, rv) =
  let rv' = RV.put n x rv
  in maybe False (RV.put n y rv' ==) (RV.modify n (const y) rv')
     
-- Note the relvars must not already contain the name.
prop_modifyOnAbsentName :: (RelName, Relation Int, RV.RelVars Int) -> Bool
prop_modifyOnAbsentName (n, x, rv) =
  maybe True (const False) (RV.modify n (const x) rv)