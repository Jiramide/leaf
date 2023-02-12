module Todo.Todo
  ( TodoItem(..)
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
import Data.Time (ZonedTime, zonedTimeToUTC)

data TodoItem
  = Leaf { title :: String, after :: [TodoItem] }
  | Branch
      { title :: String
      , description :: String
      , dueDate :: ZonedTime
      , after :: [TodoItem]
      }
  | Root
      { title :: String
      , description :: String
      , startDate :: ZonedTime
      , endDate :: ZonedTime
      , after :: [TodoItem]
      }
  deriving (Show)

isLeaf :: TodoItem -> Bool
isLeaf (Leaf _ _) = True
isLeaf _ = False

isBranch :: TodoItem -> Bool
isBranch (Branch _ _ _ _) = True
isBranch _ = False

isRoot :: TodoItem -> Bool
isRoot x = not $ isLeaf x || isBranch x

createOrderM :: TodoItem -> State (Set TodoItem) [TodoItem]
createOrderM todo = do
  visited <- get
  if (todo `member` visited)
    then return []
    else do
      modify $ insert todo
      subOrders <- forM (after todo) createOrderM
      return $ mconcat subOrders `mappend` [todo]

createOrderMany :: [TodoItem] -> [TodoItem]
createOrderMany = mconcat . (`evalState` empty) . mapM createOrderM

createOrder :: TodoItem -> [TodoItem]
createOrder = (`evalState` empty) . createOrderM

shouldBeAfter :: TodoItem -> TodoItem -> Bool
shouldBeAfter todo before
  = let dependents = after todo
    in before `elem` dependents || any (`shouldBeAfter` before) dependents
