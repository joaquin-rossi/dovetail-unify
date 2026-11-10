module DovetailSpec (spec) where

import Test.Hspec
import Data.Map.Strict qualified as Map

import Dovetail

spec :: Spec
spec = do
  let lhs = Fun "f" [atom "a", Var "Y"]
      rhs = Fun "f" [Var "X", atom "b"]
      s = Map.fromList [("X", atom "a"), ("Y", atom "b")]

  it "unify works" $ do
    unify (Equation lhs rhs) `shouldBe` Right s

  it "applySubst works" $ do
    applySubst s lhs `shouldBe` Fun "f" [atom "a", atom "b"]
    applySubst s rhs `shouldBe` Fun "f" [atom "a", atom "b"]
