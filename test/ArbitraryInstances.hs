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

arbitraryString :: Gen String
arbitraryString = listOf $ arbitraryPrintable
  where arbitraryPrintable
          = oneof
              [ choose ('A', 'Z')
              , choose ('a', 'z')
              , choose ('0', '9')
              ]

instance Arbitrary TimeZone where
  arbitrary = TimeZone <$> arbitrary <*> arbitrary <*> arbitraryString

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
        <$> arbitraryString -- title
        <*> pure [] -- after

      branch
        = Branch
        <$> arbitraryString -- title
        <*> arbitraryString -- descripton
        <*> arbitrary -- dueDate
        <*> pure [] -- after

      root
        = Root
        <$> arbitraryString -- title
        <*> arbitraryString -- description
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
      shrinkDownwards (Leaf _ dependencies) = dependencies
      shrinkDownwards (Branch name _ _ dependencies) = [Leaf name dependencies]
      shrinkDownwards (Root name desc _ end dependencies) = [Branch name desc end dependencies]
