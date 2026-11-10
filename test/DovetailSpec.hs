module DovetailSpec (spec) where

import Test.Hspec
import Data.Map.Strict qualified as Map

import Dovetail

spec :: Spec
spec = do
  let lhs = TFun "f" [atom "a", TVar "Y"]
      rhs = TFun "f" [TVar "X", atom "b"]
      s = Map.fromList [("X", atom "a"), ("Y", atom "b")]

  it "unify works" $ do
    unify (Equation lhs rhs) `shouldBe` Right s

  it "applySubst works" $ do
    applySubst s lhs `shouldBe` TFun "f" [atom "a", atom "b"]
    applySubst s rhs `shouldBe` TFun "f" [atom "a", atom "b"]
