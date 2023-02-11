module Todo.Safe
  ( safeDescription
  , safeDueDate
  , safeStartDate
  , safeEndDate
  ) where

import Todo.Todo

safeDescription :: TodoItem -> Maybe String
safeDescription todo
  | isLeaf todo = Nothing
  | otherwise = Just $ description todo

safeDueDate :: TodoItem -> Maybe Date
safeDueDate todo
  | isBranch todo = Just $ dueDate todo
  | otherwise = Nothing

safeStartDate :: TodoItem -> Maybe Date
safeStartDate todo
  | isRoot todo = Just $ startDate todo
  | otherwise = Nothing

safeEndDate :: TodoItem -> Maybe Date
safeEndDate todo
  | isRoot todo = Just $ endDate todo
  | otherwise = Nothing
