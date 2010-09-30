{-# LANGUAGE FlexibleContexts #-}

module ConditionGen where

import Control.Monad (liftM, liftM2, liftM3, foldM, join)
import Control.Monad.Error (throwError)
import Data.List (nub, sort, foldl')
import qualified Data.Map as DM
import Data.Maybe (catMaybes, mapMaybe)
import Test.QuickCheck

import Relational.Condition
import MonadUtil (untilM)

expression :: (Arbitrary d, CoArbitrary [d], Arbitrary (m d)) => Gen n -> Int -> Gen (Expression n d m)
expression name size =
    if size <= 1
    then oneof [ const, ref ]
    else oneof [ const, ref, call ]
    where const = ExpConst `liftM` arbitrary
          ref = ExpValueOf `liftM` name
          call = liftM2 ExpCall arbitrary (listOf subExpression)
          subExpression = expression name (size `div` 2)

instance Arbitrary RelOp where
    arbitrary = elements [RelLT, RelEq, RelGT]

condition :: (Arbitrary d, CoArbitrary [d], Arbitrary (m d), Arbitrary (m Bool)) => Gen n -> Int -> Gen (Condition n d m)
condition name size =
    if size <= 1
    then elements [ CondTrue, CondFalse ]
    else oneof [ return CondTrue,
                 return CondFalse,
                 CondNot `liftM` subCondition,
                 liftM2 CondAnd subCondition subCondition,
                 liftM2 CondOr subCondition subCondition,
                 liftM3 CondRel arbitrary subExpression subExpression,
                 liftM2 CondCall arbitrary (listOf subExpression)]
    where subCondition = condition name (size `div` 2)
          subExpression = expression name (size `div`2)

satisfiableCondition :: (Ord n, Show n, Ord d, Bounded d, Enum d, Arbitrary d, CoArbitrary d) =>
                        [n] -> Int -> Gen (Condition n d (Either String))
satisfiableCondition names size =
    untilM isSatisfiable (condition (elements names) size)

isSatisfiable :: (Ord n, Ord d, Bounded d, Enum d) => Condition n d (Either String) -> Bool
isSatisfiable c = maybe False (const True) (thatSatisfy names c)
    where names = attrNamesIn c

attrNamesIn c =
    case c of CondNot c' -> attrNamesIn c'
              CondAnd c' c'' -> attrNamesIn c' ++ attrNamesIn c''
              CondOr c' c'' -> attrNamesIn c' ++ attrNamesIn c''
              CondRel _ e e' -> attrNamesInExp e ++ attrNamesInExp e'
              CondCall _ exps -> concatMap attrNamesInExp exps
              _ -> []
    where attrNamesInExp e =
              case e of ExpValueOf n -> [n]
                        ExpCall _ exps -> concatMap attrNamesInExp exps
                        _ -> []

thatSatisfy :: (Ord n, Ord d, Bounded d, Enum d) =>
               [n] -> Condition n d (Either String) -> Maybe (Gen [(n,d)])
thatSatisfy names c = if null satisfying' then Nothing else Just (elements satisfying')
    where satisfying' = map DM.toList $ satisfying c $ allTuples names

satisfying :: (Ord n, Ord d) =>
              Condition n d (Either String) -> [DM.Map n d] -> [DM.Map n d]
satisfying c = filter (\m -> evalOn m)
    where evalOn m = either (const False) id (evalCondition (lookup m) c)
          lookup m n = maybe (noSuchName n) return (DM.lookup n m)
          noSuchName n = throwError "Bad name (ignored)"

allTuples :: (Bounded d, Enum d, Ord n) => [n] -> [DM.Map n d]
allTuples [] = [DM.empty]
allTuples names =
    let names' = reverse $ nub $ sort names
        sets = replicate (length names') [minBound .. maxBound]
        singletons = map (:[]) (head sets)
        tuples = foldl' (liftM2 (flip (:))) singletons (tail sets)
    in map (DM.fromList . reverse . zip names') tuples

data Small = One | Two | Three | Four | Five deriving (Eq, Ord, Show, Bounded, Enum)

instance Arbitrary Small where
    arbitrary = elements [minBound..maxBound]

instance CoArbitrary Small where
    coarbitrary s = variant (fromEnum s)
