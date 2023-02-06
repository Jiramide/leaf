module Todo
  ( Date
  , TodoItem(..)
  , TodoList
  , isLeaf
  , isBranch
  , isRoot
  , createOrder
  ) where

import Control.Monad.State
  ( State
  , runState
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

createOrder :: TodoItem -> [TodoItem]
createOrder todo = fst $ runState (createOrder' todo) empty
  where
    createOrder' :: TodoItem -> State (Set TodoItem) [TodoItem]
    createOrder' todo' = do
      visited <- get
      if (member todo' visited)
        then return []
        else do
          modify $ insert todo'
          subOrders <- forM (after todo') createOrder' 
          return $ mconcat subOrders `mappend` [todo']
