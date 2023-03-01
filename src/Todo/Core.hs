{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Todo.Core
  ( TodoStatus(..)
  , Dependencies
  , TodoItemId
  , TodoTitle
  , TodoTime
  , TodoList
  , TodoItem(..)
  , TodoType(..)
  ) where

import qualified Data.Text as Text
import Data.Ord (comparing)
import Data.Time
  ( ZonedTime
  , zonedTimeToUTC
  )

import GHC.Generics

type Dependencies = [TodoItemId]
type TodoItemId = Integer
type TodoTitle = Text.Text
type TodoTime = ZonedTime
type TodoList = [TodoItem]

data TodoStatus
  = NotStarted
  | InProgress
  | Delayed
  | Finished
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data TodoItem
  = TodoItem
      { todoId :: TodoItemId
      , todoTitle :: TodoTitle
      , todoType :: TodoType
      , todoStatus :: TodoStatus
      , todoDependencies :: Dependencies
      }
  deriving (Show, Eq, Ord, Generic)

data TodoType
  = Leaf
  | Branch
      TodoTitle -- description
      TodoTime -- dueDate
  | Root
      TodoTitle -- description
      TodoTime -- startDate
      TodoTime -- endDate
  deriving (Show, Generic)

instance Eq TodoType where
  a == b = a `compare` b == EQ

instance Ord TodoType where
  Leaf `compare` Leaf = EQ
  Leaf `compare` _ = LT

  Branch _ _ `compare` Leaf = GT
  Branch desc due `compare` Branch desc' due'
     = compare desc desc'
     <> comparing zonedTimeToUTC due due'
  Branch _ _ `compare` _ = LT

  Root desc start end `compare` Root desc' start' end'
    = compare desc desc'
    <> comparing zonedTimeToUTC start start'
    <> comparing zonedTimeToUTC end end'
  Root _ _ _ `compare` _ = GT
