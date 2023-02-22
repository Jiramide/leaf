module Todo.TodoSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (modifyMaxSuccess, modifyMaxSize)
import Data.Time (getZonedTime)
import Todo.Todo
import ArbitraryInstances ()

isTopological :: [TodoItem] -> Bool
isTopological [] = True
isTopological (x:xs) = locallyTopological && isTopological xs
  where
    locallyTopological = not . any (shouldBeAfter x) $ xs

spec :: Spec
spec = do
  describe "shouldBeAfter" $ do
    it "can properly determine that two TodoItems are direct dependents." $ do
      time <- getZonedTime
      (Leaf "after" [Leaf "before" []] `shouldBeAfter` Leaf "before" []) `shouldBe` True
      (Branch "test" "desc" time [Leaf "test" []] `shouldBeAfter` Leaf "test" []) `shouldBe` True

    it "should properly determine that two TodoItems are indirectly dependent." $ do
      time <- getZonedTime
      (Branch
        "Test"
        "A branching branch"
        time
        [ Leaf "Leafy leaves" [Leaf "Only child" []]
        , Leaf "Uncle Leaf" []
        ]
        `shouldBeAfter`
          Leaf "Only child" []
        ) `shouldBe` True

    it "should properly determine that two TodoItems are independent." $ do
      (Leaf "Independent Leaf" [] `shouldBeAfter` Leaf "Independent Leafet" [])
        `shouldBe` False
      (Leaf "Independent Leafet" [Leaf "She's a single mother" []]
        `shouldBeAfter` Leaf "Independent Leaf" [Leaf "He adopted an orphan leaf." []])
        `shouldBe` False

  describe "isLeaf" $ do
    it "can correctly detect Leaf values." $ do
      isLeaf (Leaf "test" []) `shouldBe` True
      isLeaf (Leaf "not a leaf" [Leaf "leaf" []]) `shouldBe` True

    it "can correctly disqualify Roots and Branches." $ do
      time <- getZonedTime
      isLeaf (Branch
        "test test"
        "I'm a branch constructed for testing!"
        time
        [ Leaf "I'm a child!" [] ]
        ) `shouldBe` False

  modifyMaxSuccess (const 1000) $ modifyMaxSize (const 7) $ describe "createOrder" $ do
    it "always creates a topological order" $ property $ \x ->
      isTopological (createOrder x) `shouldBe` True

  modifyMaxSuccess (const 100) $ modifyMaxSize (const 7) $ describe "createOrderMany" $ do
    it "always creates a topological order" $ property $ \x ->
      isTopological (createOrderMany x) `shouldBe` True
