module MatchSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (Fun, variant)

import Dovetail
import Support.Generators
import Support.Terms

spec :: Spec
spec = do
  describe "match" $ do
    it "binds the variables of the pattern" $ do
      match (f [x, g [y, b]]) (f [a, g [c, b]])
        `shouldBe` Just (Map.fromList [("X", a), ("Y", c)])

    it "requires repeated variables to match the same term" $ do
      match (f [x, x]) (f [a, a]) `shouldBe` Just (Map.fromList [("X", a)])
      match (f [x, x]) (f [a, b]) `shouldBe` Nothing

    it "treats variables in the target as constants" $ do
      match a x `shouldBe` Nothing
      match (f [x, y]) (f [y, a]) `shouldBe` Just (Map.fromList [("X", y), ("Y", a)])

    it "drops trivial bindings" $ do
      match (f [x, y]) (f [x, a]) `shouldBe` Just (Map.fromList [("Y", a)])

    it "fails on different functors or arities" $ do
      match (f [x]) (g [a]) `shouldBe` Nothing
      match (f [x]) (f [a, b]) `shouldBe` Nothing

    prop "returns a substitution that turns the pattern into the target" $
      forAllTerms2 $ \pat target -> case match pat target of
        Just sub -> applySubst sub pat === target
        Nothing -> property True

    prop "matches every instance of the pattern" $
      forAll genSubst $ \theta -> forAllTerm $ \pat ->
        let target = applySubst theta pat
         in fmap (`applySubst` pat) (match pat target) === Just target

    prop "only binds variables of the pattern" $
      forAllTerms2 $ \pat target -> case match pat target of
        Just sub -> property (Map.keysSet sub `Set.isSubsetOf` vars pat)
        Nothing -> property True

  describe "subsumes" $ do
    it "holds when the second term is an instance of the first" $ do
      subsumes (f [x, y]) (f [a, g [b]]) `shouldBe` True
      subsumes x (f [a]) `shouldBe` True
      subsumes (f [x, x]) (f [y, y]) `shouldBe` True

    it "does not hold the other way around" $ do
      subsumes (f [a, g [b]]) (f [x, y]) `shouldBe` False
      subsumes (f [x, x]) (f [x, y]) `shouldBe` False

  describe "variant" $ do
    it "holds for terms equal up to renaming" $ do
      variant (f [x, y, x]) (f [z, x, z]) `shouldBe` True

    it "does not hold for terms that are not renamings" $ do
      variant (f [x, y]) (f [x, x]) `shouldBe` False
      variant (f [x]) (f [a]) `shouldBe` False

    prop "holds for a renaming of a term" $
      forAll genRenaming $ \ren -> forAllTerm $ \t ->
        variant t (applySubst ren t)

    prop "is symmetric" $
      forAllTerms2 $ \t1 t2 -> variant t1 t2 === variant t2 t1
