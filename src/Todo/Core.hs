module Todo.Core
  ( TodoItem(..)
  , TodoList
  ) where

import Data.Time
  ( ZonedTime
  , zonedTimeToUTC
  )

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

type TodoList = [TodoItem]
