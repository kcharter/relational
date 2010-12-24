{-| An AST for conditions used in selections and joins. -}

module Relational.Condition (Condition(..),
                             RelOp(..),
                             Expression(..),
                             evalCondition) where

import Control.Monad (liftM, liftM2)
import Data.List (intercalate)

import Relational.ColName (ColName)

-- | Selection conditions, used in selections and joins.
-- This is an abstract syntax for a simple language of boolean
-- conditions on tuples. In practice, the third type parameter
-- @m@ should be some kind of error monad.
data Condition d m =
    CondTrue |
    -- ^ The true constant.
    CondFalse |
    -- ^ The false constant.
    CondNot (Condition d m) |
    -- ^ Logical not.
    CondAnd (Condition d m) (Condition d m) |
    -- ^ Logical and.
    CondOr (Condition d m) (Condition d m) |
    -- ^ Logical or.
    CondRel RelOp (Expression d m) (Expression d m) |
    -- ^ A relational test on two data expressions.
    CondCall ([d] -> m Bool) [Expression d m]
    -- ^ The result of calling a test function on a list of
    -- argument values.

instance (Show d) => Show (Condition d m) where
    show CondTrue = "true"
    show CondFalse = "false"
    show (CondNot c) = "not (" ++ show c ++ ")"
    show (CondAnd c c') = "(" ++ show c ++ " and " ++ show c' ++ ")"
    show (CondOr c c') = "(" ++ show c ++ " or " ++ show c' ++ ")"
    show (CondRel op e e') = "(" ++ show e ++ " " ++ show op ++ " " ++ show e' ++ ")"
    show (CondCall _ exps) = "<func>(" ++ intercalate "," (map show exps) ++ ")" 

-- | The fundamental relational operators on attribute values.
data RelOp = RelLT |
             -- ^ Less than.
             RelEq |
             -- ^ Equals.
             RelGT
             -- ^ Greater than.
             deriving (Eq, Ord)

instance Show RelOp where
    show RelLT = "<"
    show RelEq = "="
    show RelGT = ">"

-- | An expression that produces a data value from an implicit tuple.
data Expression d m =
    ExpConst d |
    -- ^ A constant data value.
    ExpValueOf ColName |
    -- ^ The value of the named attribute on the tuple.
    ExpCall ([d] -> m d) [Expression d m]
    -- ^ The result of calling a function on a list of argument
    -- expressions.

instance (Show d) => Show (Expression d m) where
    show (ExpConst x) = show x
    show (ExpValueOf n) = show n
    show (ExpCall _ exps) = "<func>(" ++ intercalate "," (map show exps) ++ ")"

-- | Evaluates a condition in an arbitrary monad, given a lookup function.
-- The lookup function retrieves attribute values from a tuple.
--
-- This function is a useful utility for those relational types where
-- there is no notion of query optimization. For those types, we would
-- evaluate conditions directly without any kind of
-- transformation. The evaluation here is generic; all an
-- implementation need do is provide a lookup function for each tuple.
evalCondition :: (Monad m, Ord d) => (ColName -> m d) -> Condition d m -> m Bool
evalCondition lookup c =
    case c of
      CondTrue ->
          return True
      CondFalse ->
          return False
      CondNot c1 ->
          not `liftM` evalSub c1
      CondAnd c1 c2 ->
          liftM2 (&&) (evalSub c1) (evalSub c2)
      CondOr c1 c2 ->
          liftM2 (||) (evalSub c1) (evalSub c2)
      CondRel op e1 e2 ->
          liftM2 (opFunc op) (evalExp e1) (evalExp e2)
      CondCall f exps ->
          mapM evalExp exps >>= f
    where evalSub = evalCondition lookup
          opFunc RelLT = (<)
          opFunc RelEq = (==)
          opFunc RelGT = (>)
          evalExp exp =
              case exp of
                ExpConst v ->
                    return v
                ExpValueOf n ->
                    lookup n
                ExpCall f exps ->
                    mapM evalExp exps >>= f
