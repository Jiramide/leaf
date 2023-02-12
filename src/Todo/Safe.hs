module Todo.Safe
  ( safeDescription
  , safeDueDate
  , safeStartDate
  , safeEndDate
  ) where

import Todo.Todo
import Data.Time (ZonedTime)

safeDescription :: TodoItem -> Maybe String
safeDescription todo
  | isLeaf todo = Nothing
  | otherwise = Just $ description todo

safeDueDate :: TodoItem -> Maybe ZonedTime
safeDueDate todo
  | isBranch todo = Just $ dueDate todo
  | otherwise = Nothing

safeStartDate :: TodoItem -> Maybe ZonedTime
safeStartDate todo
  | isRoot todo = Just $ startDate todo
  | otherwise = Nothing

safeEndDate :: TodoItem -> Maybe ZonedTime
safeEndDate todo
  | isRoot todo = Just $ endDate todo
  | otherwise = Nothing
