module ArbitraryInstances () where

import Todo.Core
  ( TodoItem(..)
  )

import Data.Time
  ( ZonedTime(..)
  , TimeZone(..)
  , TimeOfDay(..)
  , LocalTime(..)
  , Day
  )

import Data.Time.Calendar.OrdinalDate
  ( fromOrdinalDate
  )

import Test.QuickCheck

instance Arbitrary TimeZone where
  arbitrary = TimeZone <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Day where
  arbitrary = fromOrdinalDate <$> arbitrary <*> arbitrary

instance Arbitrary TimeOfDay where
  arbitrary = TimeOfDay <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitrary

instance Arbitrary ZonedTime where
  arbitrary = ZonedTime <$> arbitrary <*> arbitrary

instance Arbitrary TodoItem where
  arbitrary = sized go
    where
      leaf
        = Leaf
        <$> arbitrary -- title
        <*> pure [] -- after

      branch
        = Branch
        <$> arbitrary -- title
        <*> arbitrary -- descripton
        <*> arbitrary -- dueDate
        <*> pure [] -- after

      root
        = Root
        <$> arbitrary -- title
        <*> arbitrary -- description
        <*> arbitrary -- startDate
        <*> arbitrary -- endDate
        <*> pure [] -- after

      go 0 = oneof [leaf, branch, root]
      go n = do
        node <- oneof [leaf, branch, root]
        dependencies <- listOf . go $ n - 1
        return $ node { after = dependencies }

  shrink x = shrinkDownwards x ++ genericShrink x
    where
      shrinkDownwards (Leaf _ after) = after
      shrinkDownwards (Branch title _ _ after) = [Leaf title after]
      shrinkDownwards (Root title desc _ end after) = [Branch title desc end after]
