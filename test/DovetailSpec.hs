module DovetailSpec (spec) where

import Test.Hspec
import Data.Map.Strict qualified as Map

import Dovetail

spec :: Spec
spec = do
  let lhs = TTerm "f" [atom "1", TVar "y"]
      rhs = TTerm "f" [TVar "x", atom "2"]
      s = Map.fromList [("x", atom "1"), ("y", atom "2")]

  it "unify works" $ do
    unify lhs rhs `shouldBe` Right s

  it "applySubst works" $ do
    applySubst s lhs `shouldBe` TTerm "f" [atom "1", atom "2"]
    applySubst s rhs `shouldBe` TTerm "f" [atom "1", atom "2"]
