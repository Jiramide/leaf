module Todo
  (
  ) where

data TodoList
  = Leaf
  | Branch
  | Root
  deriving (Show, Read, Eq)


