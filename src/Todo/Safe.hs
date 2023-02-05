module Todo.Safe
  ( safeDescription
  , safeDueDate
  , safeStartDate
  , safeEndDate
  ) where

import Todo

safeDescription :: TodoList -> Maybe String
safeDescription todo
  | isLeaf todo = Nothing
  | otherwise = Just $ description todo

safeDueDate :: TodoList -> Maybe Date
safeDueDate todo
  | isBranch todo = Just $ dueDate todo
  | otherwise = Nothing

safeStartDate :: TodoList -> Maybe Date
safeStartDate todo
  | isRoot todo = Just $ startDate todo
  | otherwise = Nothing

safeEndDate :: TodoList -> Maybe Date
safeEndDate todo
  | isRoot todo = Just $ endDate todo
  | otherwise = Nothing
