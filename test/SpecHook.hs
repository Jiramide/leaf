module SpecHook (hook) where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, modifyMaxSize)

hook :: Spec -> Spec
hook
  = modifyMaxSuccess (const 100)
  . modifyMaxSize (const 7)
  . parallel
