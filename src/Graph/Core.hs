{-# LANGUAGE DeriveGeneric #-}

module Graph.Core
  ( Graph (..)
  ) where

import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map

newtype Graph c v
  = Graph { graph :: Map.Map v (Map.Map v c) }
  deriving (Eq, Show, Generic)
