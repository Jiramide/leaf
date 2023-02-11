module Todo.Todo
  ( Date(..)
  , TodoItem(..)
  , TodoList
  , isLeaf
  , isBranch
  , isRoot
  , createOrderMany
  , createOrder
  , shouldBeAfter
  ) where

import Control.Monad.State
  ( State
  , runState
  , evalState
  , get
  , put
  , modify
  , forM
  )
import Data.Set
  ( Set
  , empty
  , insert
  , member
  )

data Date = Date deriving (Show, Read, Eq, Ord)

data TodoItem
  = Leaf { title :: String, after :: [TodoItem] }
  | Branch
      { title :: String
      , description :: String
      , dueDate :: Date
      , after :: [TodoItem]
      }
  | Root
      { title :: String
      , description :: String
      , startDate :: Date
      , endDate :: Date
      , after :: [TodoItem]
      }
  deriving (Show, Read, Eq, Ord)

type TodoList = [TodoItem]

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

createOrderMany :: [TodoItem] -> TodoList
createOrderMany = mconcat . (`evalState` empty) . mapM createOrderM

createOrder :: TodoItem -> TodoList
createOrder = (`evalState` empty) . createOrderM

shouldBeAfter :: TodoItem -> TodoItem -> Bool
shouldBeAfter todo before
  = let dependents = after todo
    in before `elem` dependents || any (`shouldBeAfter` before) dependents
