module Todo.CoreSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import ArbitraryInstances

import Data.Time (getZonedTime)
import Data.Ord (comparing)

import Todo.Core
import Todo.Todo (isLeaf, isBranch, isRoot)

comparingTitle :: Property
comparingTitle
  = compareTitleBy unOnlyLeaves
  .&&. compareTitleBy unOnlyBranches
  .&&. compareTitleBy unOnlyRoots
  where
    compareTitleBy f = property $ \(x, y) ->
      let x' = f x
          y' = f y
      in title x' /= title y' ==> compare x' y' `shouldBe` comparing title x' y'

spec :: Spec
spec = do
  describe "title" $ do
    it "should return the title of a TodoItem." $ do
      time <- getZonedTime
      title (Leaf "Hello world!" []) `shouldBe` "Hello world!"
      title (Branch "Howdy" "Cool" time []) `shouldBe` "Howdy"
      title (Root "abc" "def" time time []) `shouldBe` "abc"

  describe "(==)" $ do
    it "should be reflexive." $ property $ \x ->
      (x :: TodoItem) == x `shouldBe` True

    it "should match the results of compare" $ property $ \(x, y) ->
      (compare (x :: TodoItem) y == EQ) == (x == y) `shouldBe` True

  describe "compare" $ do
    it "should compare against titles first." $ comparingTitle
