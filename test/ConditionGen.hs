{-# LANGUAGE FlexibleContexts, TupleSections #-}

module ConditionGen where

import Control.Monad (liftM, liftM2, liftM3, filterM)
import Control.Monad.Error (Error, MonadError, throwError, strMsg)
import Data.List (nub, sort, foldl')
import qualified Data.Map as DM
import Data.Maybe (catMaybes)
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

-- | Builds a generator for satisfiable conditions paried with their satisfying tuples.
satisfiableCondition :: (Ord n, Show n, Ord d, Bounded d, Enum d, Arbitrary d, CoArbitrary d) =>
                        [n] -> Int -> Gen (Condition n d (Either String), [[d]])
satisfiableCondition names size =
  untilM (not . null . snd) (conditionAndSatisfyingTuples names size)

-- | Builds a generator for conditions that are unsatisfiable.
unsatisfiableCondition :: (Ord n, Show n, Ord d, Bounded d, Enum d, Arbitrary d, CoArbitrary d) =>
                          [n] -> Int -> Gen (Condition n d (Either String))
unsatisfiableCondition names size =
  fst `liftM` untilM (null . snd) (conditionAndSatisfyingTuples names size)
  
-- | Builds a generator for conditions paired with the tuples that
-- satisfy them. A satisfiable condition will have a non-empty list of
-- satisfying tuples.
conditionAndSatisfyingTuples :: (Ord n, Show n, Ord d, Bounded d, Enum d, Arbitrary d, CoArbitrary d) =>
                                [n] -> Int -> Gen (Condition n d (Either String), [[d]])
conditionAndSatisfyingTuples names size =
  condition (elements names) size >>= \c -> (c,) `liftM` either (const (return [])) return (allSatisfying names c)
                                                                 
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

allSatisfying :: (Show n, Ord n, Ord d, Bounded d, Enum d, Error e, MonadError e m) =>
                 [n] -> Condition n d m -> m [[d]]
allSatisfying names c = map projectNames `liftM` satisfying c (allTuples names)
  where projectNames m = catMaybes $ map (flip DM.lookup m) names

satisfying :: (Show n, Ord n, Ord d, Error e, MonadError e m) =>
              Condition n d m -> [DM.Map n d] -> m [DM.Map n d]
satisfying c = filterM (evalOn c)

evalOn :: (Show n, Ord n, Ord d, Error e, MonadError e m) => Condition n d m -> DM.Map n d -> m Bool
evalOn c m = evalCondition (lookup m) c
  where lookup m n = maybe (noSuchName n) return (DM.lookup n m)
        noSuchName n = throwError $ strMsg $ "Bad name " ++ show n

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
