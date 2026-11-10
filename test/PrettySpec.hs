module PrettySpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (Fun)

import Dovetail
import Dovetail.Parser
import Dovetail.Pretty
import Support.Generators

spec :: Spec
spec = do
  describe "prettyTerm" $ do
    it "renders variables, atoms and functors" $ do
      prettyTerm (Var "X") `shouldBe` "X"
      prettyTerm (atom "a") `shouldBe` "a"
      prettyTerm (atom "-7") `shouldBe` "-7"
      prettyTerm (Fun "f" [Var "X", Fun "g" [atom "a"]]) `shouldBe` "f(X, g(a))"

    prop "is inverted by parseTerm" $
      forAllShrink genTerm shrinkTerm $ \t ->
        either (Left . show) Right (parseTerm (prettyTerm t)) === Right t

  describe "prettyEquation" $ do
    it "renders both sides" $ do
      prettyEquation (Equation (Fun "f" [Var "X"]) (atom "a")) `shouldBe` "f(X) = a"

    prop "is inverted by parseEquation" $
      forAll genTerm $ \l -> forAll genTerm $ \r ->
        let e = Equation l r
         in either (Left . show) Right (parseEquation (prettyEquation e)) === Right e

  describe "prettyEquations" $ do
    it "separates equations with commas" $ do
      prettyEquations [Equation (Var "X") (atom "a"), Equation (Var "Y") (atom "b")]
        `shouldBe` "X = a, Y = b"

    prop "is inverted by parseEquations" $
      forAll (listOf1 (Equation <$> genTerm <*> genTerm)) $ \es ->
        either (Left . show) Right (parseEquations (prettyEquations es)) === Right es

  describe "prettySubst" $ do
    it "renders bindings in order" $ do
      prettySubst (Map.fromList [("Y", atom "a"), ("X", Fun "f" [Var "Y"])])
        `shouldBe` "{X ↦ f(Y), Y ↦ a}"
      prettySubst Map.empty `shouldBe` "{}"

  describe "prettyUnifyError" $ do
    it "describes each kind of error" $ do
      prettyUnifyError (Clash "f" "g") `shouldBe` "cannot unify f with g"
      prettyUnifyError (ArityMismatch "f" 1 2) `shouldBe` "cannot unify f/1 with f/2"
      prettyUnifyError (OccursCheck "X" (Fun "f" [Var "X"]))
        `shouldBe` "occurs check: X occurs in f(X)"
