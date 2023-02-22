module Todo.TodoCoreSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (modifyMaxSuccess, modifyMaxSize)
import ArbitraryInstances

import Data.Time (getZonedTime)
import Data.Ord (comparing)

import Todo.Core
import Todo.Todo (isLeaf, isBranch, isRoot)

spec :: Spec
spec = modifyMaxSuccess (const 100) $ modifyMaxSize (const 7) $ do
  describe "title" $ do
    it "should return the title of a TodoItem." $ do
      time <- getZonedTime
      title (Leaf "Hello world!" []) `shouldBe` "Hello world!"
      title (Branch "Howdy" "Cool" time []) `shouldBe` "Howdy"
      title (Root "abc" "def" time time []) `shouldBe` "abc"

  describe "(==)" $ do
    it "should be reflexive." $ property $ \x ->
      (x :: TodoItem) == x `shouldBe` True

    it "should compare against titles first." $ property $ \(x, y) ->
      title x /= title y ==> compare x y `shouldBe` comparing title x y

