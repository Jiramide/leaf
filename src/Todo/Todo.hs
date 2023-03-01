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
todoDescription (todoType -> Branch desc _) = Just desc
todoDescription (todoType -> Root desc _ _) = Just desc

todoDueDate :: TodoItem -> Maybe TodoTime
todoDueDate (todoType -> Branch _ due) = Just due
todoDueDate _ = Nothing

todoStartDate :: TodoItem -> Maybe TodoTime
todoStartDate (todoType -> Root _ start _) = Just start
todoStartDate _ = Nothing

todoEndDate :: TodoItem -> Maybe TodoTime
todoEndDate (todoType -> Root _ _ end) = Just end
todoEndDate _ = Nothing
