module Todo.TodoList
  ( TodoList
  , getLeaves
  , getBranches
  , getRoots
  , addTodo
  , deleteTodo
  , getTodoById
  ) where

import Todo.Core
import Todo.Todo
  ( isLeaf
  , isBranch
  , isRoot
  )

getLeaves :: TodoList -> TodoList
getLeaves = filter isLeaf

getBranches :: TodoList -> TodoList
getBranches = filter isBranch

getRoots :: TodoList -> TodoList
getRoots = filter isRoot

addTodo :: TodoItem -> TodoList -> TodoList
addTodo = (:)

deleteTodo :: TodoItem -> TodoList -> TodoList
deleteTodo t = filter (/= t)

getTodoById :: TodoItemId -> TodoList -> TodoList
getTodoById t = _
