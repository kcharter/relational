{-# LANGUAGE FlexibleContexts, TupleSections #-}

module ConditionGen where

import Control.Monad (liftM, liftM2, liftM3, filterM)
import Control.Monad.Error (Error, MonadError, throwError, strMsg)
import Data.List (nub, sort, foldl')
import qualified Data.Map as DM
import Data.Maybe (catMaybes)
import Test.QuickCheck

import Relational.ColName (ColName)
import Relational.Condition
import Relational.Naive (RelationalMonad(..), evalPure)
import MonadUtil (untilM)

expression :: (Arbitrary d, CoArbitrary [d], Monad m) => Maybe (Gen ColName) -> Int -> Gen (Expression d m)
expression mName size =
    if size <= 1
    then oneof $ maybeRef ++ [ const ]
    else oneof $ maybeRef ++ [ const, call ]
    where maybeRef = maybe [] ((:[]) . liftM ExpValueOf) mName
          const = ExpConst `liftM` arbitrary
          call = liftM2 ExpCall noFailFunction args
          args = resize 4 (listOf subExpression)
          subExpression = expression mName (size `div` 2)

instance Arbitrary RelOp where
    arbitrary = elements [RelLT, RelEq, RelGT]

condition :: (Arbitrary d, CoArbitrary [d], Monad m) => Maybe (Gen ColName) -> Int -> Gen (Condition d m)
condition mName size =
    if size <= 1
    then elements [ CondTrue, CondFalse ]
    else oneof [ return CondTrue,
                 return CondFalse,
                 CondNot `liftM` subCondition,
                 liftM2 CondAnd subCondition subCondition,
                 liftM2 CondOr subCondition subCondition,
                 liftM3 CondRel arbitrary subExpression subExpression,
                 liftM2 CondCall noFailFunction args ]
    where subCondition = condition mName (size `div` 2)
          subExpression = expression mName (size `div`2)
          args = resize 4 (listOf subExpression)

  
-- | Builds an arbitrary function that returns its result in a monad.
--
-- This addresses a subtle error that I encountered when generating
-- functions in expressions and conditions. There, in practice the
-- monad is @(Either String)@, and you can use @arbitary@ directly if
-- @r@ is 'Arbitary' in @(Either String r)@. However, this means that
-- the generated function may generate errors! I found this out the
-- hard way when I discovered that 'unsatisfiableCondition' would
-- usually pick conditions with functions that threw errors!
noFailFunction :: (CoArbitrary [d], Arbitrary r, Monad m) => Gen ([d] -> m r)
noFailFunction = arbitrary >>= \f -> return (return . f)

-- | Builds a generator for satisfiable conditions paried with their satisfying tuples.
satisfiableCondition :: (Ord d, Bounded d, Enum d, Arbitrary d, CoArbitrary d, Show d) =>
                        [ColName] -> Int -> Gen (Condition d (RelationalMonad d), [[d]])
satisfiableCondition names size =
  untilM (not . null . snd) (conditionAndSatisfyingTuples names size)

-- | Builds a generator for conditions that are unsatisfiable.
unsatisfiableCondition :: (Ord d, Bounded d, Enum d, Arbitrary d, CoArbitrary d, Show d) =>
                          [ColName] -> Int -> Gen (Condition d (RelationalMonad d))
unsatisfiableCondition names size =
  fst `liftM` untilM (null . snd) (conditionAndSatisfyingTuples names size)
  
-- | Builds a generator for conditions paired with the tuples that
-- satisfy them. A satisfiable condition will have a non-empty list of
-- satisfying tuples.
conditionAndSatisfyingTuples :: (Ord d, Bounded d, Enum d, Arbitrary d, CoArbitrary d, Show d) =>
                                [ColName] -> Int -> Gen (Condition d (RelationalMonad d), [[d]])
conditionAndSatisfyingTuples names size =
  condition mNames size >>= \c -> (c,) `liftM` either (abort c) return (evalPure $ allSatisfying names c)
    where abort c = error . (("error determining all satisfying tuples for " ++ show c ++ ": ") ++) . show
          mNames = if null names then Nothing else Just (elements names)
              
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

allSatisfying :: (Ord d, Bounded d, Enum d, Error e, MonadError e m) =>
                 [ColName] -> Condition d m -> m [[d]]
allSatisfying names c = map projectNames `liftM` satisfying c (allTuples names)
  where projectNames m = catMaybes $ map (flip DM.lookup m) names

satisfying :: (Ord d, Error e, MonadError e m) =>
              Condition d m -> [DM.Map ColName d] -> m [DM.Map ColName d]
satisfying c = filterM (evalOn c)

evalOn :: (Ord d, Error e, MonadError e m) => Condition d m -> DM.Map ColName d -> m Bool
evalOn c m = evalCondition (lookup m) c
  where lookup m n = maybe (noSuchName n) return (DM.lookup n m)
        noSuchName n = throwError $ strMsg $ "Bad name " ++ show n

allTuples :: (Bounded d, Enum d) => [ColName] -> [DM.Map ColName d]
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
