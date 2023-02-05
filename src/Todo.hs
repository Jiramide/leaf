module Todo
  ( TodoList
  , createOrder
  ) where

import Data.Maybe
import Control.Monad.State
import qualified Data.Set as Set

data Date = Date deriving (Show, Read, Eq, Ord)

data TodoList
  = Leaf { title :: String, after :: [TodoList] }
  | Branch
      { title :: String
      , description :: String
      , dueDate :: Date
      , after :: [TodoList]
      }
  | Root
      { title :: String
      , description :: String
      , startDate :: Date
      , endDate :: Date
      , after :: [TodoList]
      }
  deriving (Show, Read, Eq, Ord)

isLeaf :: TodoList -> Bool
isLeaf (Leaf _ _) = True
isLeaf _ = False

isBranch :: TodoList -> Bool
isBranch (Branch _ _ _ _) = True
isBranch _ = False

isRoot :: TodoList -> Bool
isRoot x = not $ isLeaf x || isBranch x

