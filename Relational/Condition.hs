{-| An AST for conditions used in selections and joins. -}

module Relational.Condition (Condition(..), RelOp(..), Expression(..)) where

-- | Selection conditions, used in selections and joins.
-- This is an abstract syntax for a simple language of boolean
-- conditions on tuples. In practice, the third type parameter
-- should be some kind of error monad.
data Condition n d m =
    CondTrue |
    -- ^ The true constant.
    CondFalse |
    -- ^ The false constant.
    CondNot (Condition n d m) |
    -- ^ Logical not.
    CondAnd (Condition n d m) (Condition n d m) |
    -- ^ Logical and.
    CondOr (Condition n d m) (Condition n d m) |
    -- ^ Logical or.
    CondRel RelOp (Expression n d m) (Expression n d m) |
    -- ^ A relational test on two data expressions.
    CondTupleTest ((n -> m d) -> m Bool)
    -- ^ Holds an arbitrary boolean-valued test.
    -- The test takes a lookup function that maps attribute
    -- names to values in 'm' and returns a boolean in 'm'.

-- | The fundamental relational operators on attribute values.
data RelOp = RelLT | RelEq | RelGT deriving (Eq, Ord, Show)

-- | An expression that produces a data value from an implicit tuple.
data Expression n d m =
    ExpConst d |
    -- ^ A constant data value.
    ExpValueOf n |
    -- ^ The value of the named attribute on the tuple.
    ExpCall ([d] -> m d) [Expression n d m]
    -- ^ The result of calling a function on a list of argument
    -- expressions.

