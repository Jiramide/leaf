module Todo.TodoListSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (modifyMaxSuccess, modifyMaxSize)
import Todo.Todo
import Todo.TodoList
import ArbitraryInstances

spec :: Spec
spec = modifyMaxSuccess (const 100) $ modifyMaxSize (const 7) $ do
  describe "getLeaves" $ do
    it "should always only return leaves." $ property $ \x -> do
      getLeaves x `shouldSatisfy` all isLeaf
    
    it "should never remove leaves." $ property $ \x -> do
      let x' = fmap unOnlyLeaves x
      getLeaves x' `shouldBe` x'

    it "should always remove branches." $ property $ \x -> do
      let x' = fmap unOnlyBranches x
      getLeaves x' `shouldBe` []

    it "should always remove roots." $ property $ \x -> do
      let x' = fmap unOnlyRoots x
      getLeaves x' `shouldBe` []

  describe "getBranches" $ do
    it "should always only return branches." $ property $ \x -> do
      getBranches x `shouldSatisfy` all isBranch
    
    it "should always remove leaves." $ property $ \x -> do
      let x' = fmap unOnlyLeaves x
      getBranches x' `shouldBe` []

    it "should never remove branches." $ property $ \x -> do
      let x' = fmap unOnlyBranches x
      getBranches x' `shouldBe` x'

    it "should always remove roots." $ property $ \x -> do
      let x' = fmap unOnlyRoots x
      getBranches x' `shouldBe` []

  describe "getRoots" $ do
    it "should always only return roots." $ property $ \x -> do
      getRoots x `shouldSatisfy` all isRoot
    
    it "should always remove leaves." $ property $ \x -> do
      let x' = fmap unOnlyLeaves x
      getRoots x' `shouldBe` []

    it "should always remove branches." $ property $ \x -> do
      let x' = fmap unOnlyBranches x
      getRoots x' `shouldBe` []

    it "should never remove roots." $ property $ \x -> do
      let x' = fmap unOnlyRoots x
      getRoots x' `shouldBe` x'
