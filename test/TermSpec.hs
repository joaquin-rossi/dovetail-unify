module TermSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (Fun, variant)

import Dovetail
import Support.Generators
import Support.Terms

spec :: Spec
spec = do
  describe "vars" $ do
    it "collects the variables at any depth" $ do
      vars (f [x, g [y, a], x]) `shouldBe` Set.fromList ["X", "Y"]
      vars (g [a, b]) `shouldBe` Set.empty

  describe "occursVar" $ do
    it "finds a variable at any depth" $ do
      occursVar "X" (f [a, g [b, x]]) `shouldBe` True
      occursVar "X" (f [y, Fun "X" []]) `shouldBe` False

  describe "rename" $ do
    it "renames only the variables in the set" $ do
      rename (Set.fromList ["X"]) (f [x, y])
        `shouldBe` (f [Var "X1", y], Map.fromList [("X", Var "X1")])

    it "avoids names in the set and in the term" $ do
      rename (Set.fromList ["X", "X1"]) (f [x])
        `shouldBe` (f [Var "X2"], Map.fromList [("X", Var "X2")])
      rename (Set.fromList ["X"]) (f [x, Var "X1"])
        `shouldBe` (f [Var "X2", Var "X1"], Map.fromList [("X", Var "X2")])

    it "numbers from the name without its trailing digits" $ do
      rename (Set.fromList ["X2"]) (f [Var "X2"])
        `shouldBe` (f [Var "X1"], Map.fromList [("X2", Var "X1")])

    it "gives each renamed variable a different name" $ do
      rename (Set.fromList ["X", "X1"]) (f [x, Var "X1"])
        `shouldBe` (f [Var "X2", Var "X3"], Map.fromList [("X", Var "X2"), ("X1", Var "X3")])

    it "leaves the term unchanged when nothing clashes" $ do
      rename (Set.fromList ["Z"]) (f [x, y]) `shouldBe` (f [x, y], Map.empty)

    prop "returns a variant of the term" $
      forAll genAvoid $ \avoid -> forAllTerm $ \t ->
        variant t (fst (rename avoid t))

    prop "returns a term with no variables in the set" $
      forAll genAvoid $ \avoid -> forAllTerm $ \t ->
        Set.disjoint (vars (fst (rename avoid t))) avoid

    prop "returns the renaming it applied" $
      forAll genAvoid $ \avoid -> forAllTerm $ \t ->
        let (t', ren) = rename avoid t
         in applySubst ren t === t'

    prop "only renames variables in the set" $
      forAll genAvoid $ \avoid -> forAllTerm $ \t ->
        Map.keysSet (snd (rename avoid t)) === vars t `Set.intersection` avoid
  where
    genAvoid :: Gen (Set Name)
    genAvoid = Set.fromList <$> sublistOf ["X", "Y", "Z", "W", "X1", "Y2"]
