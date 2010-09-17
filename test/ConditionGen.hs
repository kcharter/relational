{-# LANGUAGE FlexibleContexts #-}

module ConditionGen where

import Control.Monad (liftM, liftM2, liftM3)
import Test.QuickCheck

import Relational.Condition

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