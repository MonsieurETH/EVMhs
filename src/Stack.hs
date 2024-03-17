-- Based on the implementation of https://hackage.haskell.org/package/Stack-0.4.0/docs/src/Data.Stack.html

-- | Stack data structure and associated operations
--
-- A stack is a basic data structure that can be logically thought as linear structure represented by a real physical stack or pile, a structure where insertion and deletion of items takes place at one end called top of the stack.
--
-- In other words, a 'Stack' is an abstract data type that serves as a collection of elements, with two principal operations: 'stackPush', which adds an element to the collection, and 'stackPop', which removes the most recently added element that was not yet removed.
--
-- <<https://upload.wikimedia.org/wikipedia/commons/b/b4/Lifo_stack.png>>
--
-- See also <https://en.wikipedia.org/wiki/Stack_(abstract_data_type)>
module Stack
  ( Stack,
    stackNew,
    stackPush,
    stackPeek,
    stackPop,
    stackIsEmpty,
    stackSize,
    stackPeekN,
    stackSwapNM,
  )
where

import Control.DeepSeq
import Numeric.Natural

-- | Abstract Stack data type
data Stack a = Stack !Natural [a] deriving (Read, Show)

instance (NFData a) => NFData (Stack a) where
  rnf (Stack sz items) = sz `deepseq` items `deepseq` ()

instance Semigroup (Stack a) where
  (Stack sz1 items1) <> (Stack sz2 items2) = Stack (sz1 + sz2) (items1 <> items2)

instance Monoid (Stack a) where
  mempty = Stack 0 []

-- | /O(1)/. Create new empty Stack
stackNew :: Stack a
stackNew = Stack 0 []

-- | /O(1)/. Push item onto Stack
--
-- > (∀x)(∀s)(stackPop (stackPush s x) == Just (s,x))
stackPush :: Stack a -> a -> Stack a
stackPush (Stack sz items) item = Stack (succ sz) (item : items)

-- | /O(1)/. Pop most recently added item without removing from the Stack
--
-- > stackPeek stackNew == Nothing
-- > (∀x)(∀s)(stackPeek (stackPush s x) == Just x)
-- > (∀s)(stackPeek s == fmap snd (stackPop s))
stackPeek :: Stack a -> Maybe a
stackPeek (Stack _ []) = Nothing
stackPeek (Stack _ items) = Just (head items)

-- | /O(1)/. Pop most recently added item from Stack
--
-- > stackPop stackNew == Nothing
-- > (∀x)(∀s)(stackPop (stackPush s x) == Just (s,x))
stackPop :: Stack a -> Maybe (Stack a, a)
stackPop (Stack _ []) = Nothing
stackPop (Stack sz items) = Just (Stack (pred sz) (tail items), head items)

-- | /O(1)/. Test if stack is empty
--
-- > stackIsEmpty stackNew == True
-- > (∀x)(∀s)(stackIsEmpty (stackPush s x) == True)
-- > (∀s)((stackSize s == 0) ⇔ (stackIsEmpty s == True))
stackIsEmpty :: Stack a -> Bool
stackIsEmpty (Stack _ []) = True
stackIsEmpty (Stack _ _) = False

-- | /O(1)/. Compute number of elements contained in the Stack
--
-- > stackSize stackNew == 0
-- > (∀x)(∀s)((stackSize s == n) ⇒ (stackSize (stackPush s x) == n+1))
stackSize :: Stack a -> Natural
stackSize (Stack sz _) = sz

stackPeekN :: Stack a -> Int -> Maybe a
stackPeekN (Stack sz items) n
  | fromIntegral n >= sz = Nothing
  | otherwise = Just (items !! n)

stackSwapNM :: Stack a -> Int -> Int -> Stack a
stackSwapNM (Stack sz items) n m
  | fromIntegral n >= sz || fromIntegral m >= sz = Stack sz items
  | otherwise = Stack sz (swapElements n m items)
  where
    swapElements :: Int -> Int -> [a] -> [a]
    swapElements i j xs
      | i >= 0 && j >= 0 && i < length xs && j < length xs =
          let elemI = xs !! i
              elemJ = xs !! j
              replace k x = if k == i then elemJ else if k == j then elemI else x
           in mapIndexed replace xs
      | otherwise = xs
      where
        mapIndexed f = zipWith f [0 ..]