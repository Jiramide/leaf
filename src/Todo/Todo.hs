module Todo.Todo
  ( TodoItem(..)
  , isLeaf
  , isBranch
  , isRoot
  , createOrderMany
  , createOrder
  , shouldBeAfter
  ) where

import Todo.Core
import Control.Monad.State
  ( State
  , evalState
  , get
  , modify
  , forM
  )
import Data.Set
  ( Set
  , empty
  , insert
  , member
  )

isLeaf :: TodoItem -> Bool
isLeaf (Leaf _ _) = True
isLeaf _ = False

isBranch :: TodoItem -> Bool
isBranch (Branch _ _ _ _) = True
isBranch _ = False

isRoot :: TodoItem -> Bool
isRoot x = not $ isLeaf x || isBranch x

createOrderM :: TodoItem -> State (Set TodoItem) TodoList
createOrderM todo = do
  visited <- get
  if (todo `member` visited)
    then return []
    else do
      modify $ insert todo
      subOrders <- forM (after todo) createOrderM
      return $ mconcat subOrders `mappend` [todo]

createOrderMany :: TodoList -> TodoList
createOrderMany = mconcat . (`evalState` empty) . mapM createOrderM

createOrder :: TodoItem -> TodoList
createOrder = (`evalState` empty) . createOrderM

shouldBeAfter :: TodoItem -> TodoItem -> Bool
shouldBeAfter todo before
  = let dependents = after todo
    in before `elem` dependents || any (`shouldBeAfter` before) dependents
