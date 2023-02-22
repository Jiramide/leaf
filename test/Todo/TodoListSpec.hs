module Todo.TodoListSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (modifyMaxSuccess, modifyMaxSize)
import Todo.Todo
import Todo.TodoList
import ArbitraryInstances

spec :: Spec
spec = do
  describe "getLeaves" $ do
    it "should always only return leaves." $ property $ \x ->
      getLeaves x `shouldSatisfy` all isLeaf
    
    it "should never remove leaves." $ property $ \x ->
      let x' = fmap unOnlyLeaves x in getLeaves x' `shouldBe` x'

    it "should always remove branches." $ property $ \x ->
      let x' = fmap unOnlyBranches x in getLeaves x' `shouldBe` []

    it "should always remove roots." $ property $ \x ->
      let x' = fmap unOnlyRoots x in getLeaves x' `shouldBe` []

  describe "getBranches" $ do
    it "should always only return branches." $ property $ \x ->
      getBranches x `shouldSatisfy` all isBranch
    
    it "should always remove leaves." $ property $ \x ->
      let x' = fmap unOnlyLeaves x in getBranches x' `shouldBe` []

    it "should never remove branches." $ property $ \x ->
      let x' = fmap unOnlyBranches x in getBranches x' `shouldBe` x'

    it "should always remove roots." $ property $ \x ->
      let x' = fmap unOnlyRoots x in getBranches x' `shouldBe` []

  describe "getRoots" $ do
    it "should always only return roots." $ property $ \x ->
      getRoots x `shouldSatisfy` all isRoot
    
    it "should always remove leaves." $ property $ \x ->
      let x' = fmap unOnlyLeaves x in getRoots x' `shouldBe` []

    it "should always remove branches." $ property $ \x ->
      let x' = fmap unOnlyBranches x in getRoots x' `shouldBe` []

    it "should never remove roots." $ property $ \x ->
      let x' = fmap unOnlyRoots x in getRoots x' `shouldBe` x'
