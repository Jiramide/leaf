{-# LANGUAGE ViewPatterns #-}

module Todo.Todo
  ( TodoItem(..)
  , isLeaf
  , isBranch
  , isRoot
  , todoDescription
  , todoDueDate
  , todoStartDate
  , todoEndDate
  ) where

import Todo.Core
import Control.Monad.State
  ( State
  , evalState
  , get
  , modify
  , forM
  )

isLeaf :: TodoItem -> Bool
isLeaf (todoType -> Leaf) = True
isLeaf _ = False

isBranch :: TodoItem -> Bool
isBranch (todoType -> Branch _ _) = True
isBranch _ = False

isRoot :: TodoItem -> Bool
isRoot x = not $ isLeaf x || isBranch x

todoDescription :: TodoItem -> Maybe TodoTitle
todoDescription (todoType -> Leaf) = Nothing
todoDescription (todoType -> Branch desc _) = desc
todoDescription (todoType -> Root desc _ _) = desc

todoDueDate :: TodoItem -> Maybe TodoTime
todoDueDate (todoType -> Branch _ due) = Just due
todoDueDate _ = Nothing

todoStartDate :: TodoItem -> Maybe TodoTime
todoStartDate (todoType -> Leaf _ start _) = Just start
todoStartDate _ = Nothing

todoEndDate :: TodoItem -> Maybe TodoTime
todoEndDate (todoType -> Leaf _ _ end) = Just end
todoEndDate _ = Nothing

createOrderM :: TodoItem -> TodoList -> State (Set TodoItem) [TodoItem]
createOrderM todo todoList = do
  visited <- get
  if (todo `member` visited)
    then return []
    else do
      modify $ insert todo
      subOrders <- forM (dependencies todo) createOrderM
      return $ mconcat subOrders `mappend` [todo]

createOrderMany :: TodoList -> TodoList
createOrderMany = mconcat . (`evalState` empty) . mapM createOrderM

createOrder :: TodoItem -> TodoList
createOrder = (`evalState` empty) . createOrderM

shouldBeAfter :: TodoItem -> TodoItem -> Bool
shouldBeAfter todo before
  = let dependents = after todo
    in before `elem` dependents || any (`shouldBeAfter` before) dependents
