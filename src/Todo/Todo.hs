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
import Data.Semigroup ((<>))
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

instance Eq TodoItem where
  a == b = a `compare` b == EQ

instance Ord TodoItem where
  Leaf t a `compare` Leaf t' a' = t `compare` t' <> a `compare` a'
  Leaf _ _ `compare` _ = LT

  Branch _ _ _ _ `compare` Leaf _ _ = GT
  Branch t d due a `compare` Branch t' d' due' a'
    = t `compare` t'
      <> d `compare` d'
      <> zonedTimeToUTC due `compare` zonedTimeToUTC due'
      <> a `compare` a'
  Branch _ _ _ _ `compare` Root _ _ _ _ _ = LT

  Root t d s e a `compare` Root t' d' s' e' a'
    = t `compare` t'
      <> d `compare` d'
      <> zonedTimeToUTC s `compare` zonedTimeToUTC s'
      <> zonedTimeToUTC e `compare` zonedTimeToUTC e'
      <> a `compare` a'
  Root _ _ _ _ _ `compare` _ = GT

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
