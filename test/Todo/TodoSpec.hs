module Todo.TodoSpec (spec) where

import Test.Hspec
import Data.Time (getZonedTime)
import Todo.Todo

isTopological :: [TodoItem] -> Bool
isTopological [] = True
isTopological (x:xs) = locallyTopological && isTopological xs
  where
    locallyTopological = not . any (shouldBeAfter x) $ xs

spec :: Spec
spec = do
  before getZonedTime $ do
    describe "shouldBeAfter" $ do
      it "can properly determine that two TodoItems are direct dependents." $ \time -> do
        (Leaf "after" [Leaf "before" []] `shouldBeAfter` Leaf "before" []) `shouldBe` True
        (Branch "test" "desc" time [Leaf "test" []] `shouldBeAfter` Leaf "test" []) `shouldBe` True

      it "should properly determine that two TodoItems are indirectly dependent." $ \time -> do
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

      it "should properly determine that two TodoItems are independent." $ \time -> do
        (Leaf "Independent Leaf" [] `shouldBeAfter` Leaf "Independent Leafet" [])
          `shouldBe` False
        (Leaf "Independent Leafet" [Leaf "She's a single mother" []]
          `shouldBeAfter` Leaf "Independent Leaf" [Leaf "He adopted an orphan leaf." []])
          `shouldBe` False

    describe "isLeaf" $ do
      it "can correctly detect Leaf values." $ \time -> do
        isLeaf (Leaf "test" []) `shouldBe` True
        isLeaf (Leaf "not a leaf" [Leaf "leaf" []]) `shouldBe` True

      it "can correctly disqualify Roots and Branches." $ \time -> do
        isLeaf (Branch
          "test test"
          "I'm a branch constructed for testing!"
          time
          [ Leaf "I'm a child!" [] ]
          ) `shouldBe` False
