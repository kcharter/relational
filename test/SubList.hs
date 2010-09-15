module SubList (subList) where

import Control.Monad (liftM)
import Data.List(foldl')
import Test.QuickCheck

-- | Given a list @s@ and a desired sub-list length @n@, returns a
-- | generator of sub-lists of @s@ of length @n@.
subList :: [a] -> Int -> Gen [a]
subList s n = 
    fromIndexes s `liftM` indexSubLists n (length s - 1)

-- | @fromIndexes list indexList@ returns the sub-list of
-- | @list@ whose elements are at positions given by @indexList@.
-- | @indexList@ is assumed sorted in ascending order.
fromIndexes :: [a] -> [Int] -> [a]
fromIndexes list indexList =
    let (rev,_,_) = foldl' accum ([],0,indexList) list
        accum t@(_, _, []) _ = t
        accum (sofar, i, j:iRest) x | i == j = (x:sofar, i+1, iRest)
        accum (sofar, i, indexes) _ = (sofar, i+1, indexes)
    in reverse rev


-- | Given a sub-list length @n@ and a maximum index @max@, returns a
-- | generator of ordered sub-lists of length @n@ of @[0..max]@.
indexSubLists :: Int -> Int -> Gen [Int]
indexSubLists n max =
    if n == 0 || max < 0
    then return []
    else if n > max
         then return [0..max]
         else do i <- choose (0,max)
                 left <- let maxRight = min (n - 1) (max - i)
                             minLeft = n - 1 - maxRight
                             maxLeft = min (n - 1) i
                             -- A simple case analysis proves that
                             -- it's impossible for maxLeft < minLeft
                             -- as long as 0 <= i <= max and 0 <= n <=
                             -- max+1. See the long comment following
                             -- this definition.
                         in do nLeft <- choose (minLeft,maxLeft)
                               indexSubLists nLeft (i - 1)
                 rightShifted <- indexSubLists (n - 1 - length left) (max - i - 1)
                 return (left ++ i:(map ((i+1)+) rightShifted))

{-

Here's the proof that maxLeft >= minLeft. First, we have five
assumptions and definitions

(1) 0 <= i <= max
(2) 1 <= n <= max + 1
(3) maxRight = min (n - 1) (max - i)
(4) minLeft = n - 1 - maxRight
(5) maxLeft = min (n - 1) i

Given these assumptions,

maxLeft < minLeft
<==> min (n-1) i < n-1-maxRight
<==> min (n-1) i < n-1-(min (n-1) (max-i))

Suppose min (n-1) i = n-1. Then we get

<==> n-1 < n-1 - (min (n-1) (max-i))

which is possible only if (min (n-1) (max-i)) is negative, which is
impossible given constraints (1) and (2). Therefore, in the case where

 min (n-i) i = n-1,

we cannot have maxLeft < minLeft.

So suppose min (n-1) i = i. Then we get

<==> i < n-1 - (min (n-1) (max-i))

Suppose min (n-1) (max-i) = n-1

Then we get

<==> i < n-1 - (n-1) = 0

which violates assumption (1), so we cannot have maxLeft < minLeft.

So suppose min (n-1) (max-i) = max-i.

Then we get

<==> i < n-1 - (max-i)
<==> i < n-1 - max + i
<==> 0 < n-1 - max
<==> max < n-1
==> max+1 < n

which violates assumption (2).

Therefore, as long as assumptions (1) and (2) hold, there are no cases
in which maxLeft < minLeft.

-}
