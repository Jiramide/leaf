module Todo.TodoList
  ( TodoList
  , getLeaves
  , getBranches
  , getRoots
  ) where

import Todo.Todo

type TodoList = [TodoItem]

getLeaves :: TodoList -> TodoList
getLeaves = undefined

getBranches :: TodoList -> TodoList
getBranches = undefined

getRoots :: TodoList -> TodoList
getRoots = undefined
