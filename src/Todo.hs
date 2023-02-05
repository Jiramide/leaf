module Todo
  ( TodoList
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
import qualified Data.Set as Set

data Date = Date deriving (Show, Read, Eq, Ord)

data TodoList
  = Leaf { title :: String, after :: [TodoList] }
  | Branch
      { title :: String
      , description :: String
      , dueDate :: Date
      , after :: [TodoList]
      }
  | Root
      { title :: String
      , description :: String
      , startDate :: Date
      , endDate :: Date
      , after :: [TodoList]
      }
  deriving (Show, Read, Eq, Ord)

isLeaf :: TodoList -> Bool
isLeaf (Leaf _ _) = True
isLeaf _ = False

isBranch :: TodoList -> Bool
isBranch (Branch _ _ _ _) = True
isBranch _ = False

isRoot :: TodoList -> Bool
isRoot x = not $ isLeaf x || isBranch x

createOrder :: TodoList -> [TodoList]
createOrder todo = fst $ runState (createOrder' todo) Set.empty
  where
    createOrder' :: TodoList -> State (Set.Set TodoList) [TodoList]
    createOrder' todo' = do
      visited <- get
      if (Set.member todo' visited)
        then return []
        else do
          modify $ Set.insert todo'
          subOrders <- forM (after todo') createOrder' 
          return $ mconcat subOrders `mappend` [todo']
