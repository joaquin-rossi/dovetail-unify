module SubstSpec (spec) where

import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (Fun)

import Dovetail
import Support.Generators
import Support.Terms

spec :: Spec
spec = do
  describe "applySubst" $ do
    it "leaves terms unchanged under the empty substitution" $ do
      applySubst Map.empty (f [x, g [a]]) `shouldBe` f [x, g [a]]

    it "replaces variables at any depth" $ do
      let sub = Map.fromList [("X", a), ("Y", g [b])]
      applySubst sub (f [x, g [y, x]]) `shouldBe` f [a, g [g [b], a]]

    it "leaves unbound variables unchanged" $ do
      applySubst (Map.fromList [("X", a)]) (f [x, z]) `shouldBe` f [a, z]

    it "does not replace functors that share a variable's name" $ do
      applySubst (Map.fromList [("X", a)]) (Fun "X" [x]) `shouldBe` Fun "X" [a]

    it "replaces simultaneously, without re-applying to the result" $ do
      let sub = Map.fromList [("X", y), ("Y", a)]
      applySubst sub (f [x, y]) `shouldBe` f [y, a]

  describe "composeSubst" $ do
    let sub1 = Map.fromList [("X", f [y]), ("Z", a)]
        sub2 = Map.fromList [("Y", g [z]), ("Z", b)]

    it "applies the right substitution first, then the left one" $ do
      let sub = composeSubst sub2 sub1
      for_ [x, y, z, f [x, y, z], g [Var "W"]] $ \t ->
        applySubst sub t `shouldBe` applySubst sub2 (applySubst sub1 t)

    it "applies the left substitution to the right one's bindings" $ do
      composeSubst sub2 sub1
        `shouldBe` Map.fromList [("X", f [g [z]]), ("Y", g [z]), ("Z", a)]

    it "has the empty substitution as identity" $ do
      composeSubst Map.empty sub1 `shouldBe` sub1
      composeSubst sub1 Map.empty `shouldBe` sub1

    it "drops bindings that become trivial" $ do
      composeSubst (Map.fromList [("Y", x)]) (Map.fromList [("X", y)])
        `shouldBe` Map.fromList [("Y", x)]

    it "is associative" $ do
      let sub3 = Map.fromList [("Z", y), ("W", x)]
      composeSubst sub3 (composeSubst sub2 sub1)
        `shouldBe` composeSubst (composeSubst sub3 sub2) sub1

    prop "applies the right substitution first, then the left one, for any terms" $
      forAll genSubst $ \s1 -> forAll genSubst $ \s2 -> forAllTerm $ \t ->
        applySubst (composeSubst s2 s1) t === applySubst s2 (applySubst s1 t)

    prop "is associative for any substitutions" $
      forAll genSubst $ \s1 -> forAll genSubst $ \s2 -> forAll genSubst $ \s3 ->
        composeSubst s3 (composeSubst s2 s1) === composeSubst (composeSubst s3 s2) s1

    prop "has the empty substitution as identity for any substitution" $
      forAll genSubst $ \sub ->
        let sub' = Map.filterWithKey (\n t -> t /= Var n) sub
         in composeSubst Map.empty sub === sub' .&&. composeSubst sub Map.empty === sub'
