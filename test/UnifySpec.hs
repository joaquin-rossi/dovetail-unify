module UnifySpec (spec) where

import Data.Either (isRight)
import Data.Map.Strict qualified as Map
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (Fun)

import Dovetail
import Support.Generators
import Support.Terms

spec :: Spec
spec = do
  describe "unify" $ do
    let unifies l r = do
          let result = unify l r
          case result of
            Left err -> expectationFailure ("expected a unifier, got " ++ show err)
            Right sub -> do
              applySubst sub l `shouldBe` applySubst sub r
              (applySubst sub <$> sub) `shouldBe` sub
          pure result

    it "returns the empty substitution for equal terms" $ do
      unify (f [x, a]) (f [x, a]) `shouldBe` Right Map.empty
      unify x x `shouldBe` Right Map.empty

    it "binds a variable on either side" $ do
      unify x a `shouldBe` Right (Map.fromList [("X", a)])
      unify a x `shouldBe` Right (Map.fromList [("X", a)])

    it "binds a variable to another variable" $ do
      unify x y `shouldBe` Right (Map.fromList [("X", y)])

    it "decomposes functors" $ do
      unifies (f [a, y]) (f [x, b])
        `shouldReturn` Right (Map.fromList [("X", a), ("Y", b)])
      unifies (f [g [x], y]) (f [g [a], g [x]])
        `shouldReturn` Right (Map.fromList [("X", a), ("Y", g [a])])

    it "propagates bindings to earlier variables" $ do
      unifies (f [x, y]) (f [y, a])
        `shouldReturn` Right (Map.fromList [("X", a), ("Y", a)])
      unifies (f [x, y, z]) (f [y, z, b])
        `shouldReturn` Right (Map.fromList [("X", b), ("Y", b), ("Z", b)])

    it "fails on different functors" $ do
      unify a b `shouldBe` Left (Clash "a" "b")
      unify (f [x]) (g [x]) `shouldBe` Left (Clash "f" "g")
      unify (f [g [a]]) (f [g [b]]) `shouldBe` Left (Clash "a" "b")

    it "fails on different arities" $ do
      unify (f [a]) (f [a, b]) `shouldBe` Left (ArityMismatch "f" 1 2)
      unify (g [f [a, b]]) (g [f []]) `shouldBe` Left (ArityMismatch "f" 2 0)
      unify a (f [a]) `shouldBe` Left (Clash "a" "f")

    it "fails the occurs check" $ do
      unify x (f [x]) `shouldBe` Left (OccursCheck "X" (f [x]))
      unify (g [x]) x `shouldBe` Left (OccursCheck "X" (g [x]))

    it "fails the occurs check through other bindings" $ do
      unify (f [x, y]) (f [y, g [x]])
        `shouldBe` Left (OccursCheck "Y" (g [y]))

    prop "returns a unifier" $
      forAllTerms2 $ \t1 t2 -> case unify t1 t2 of
        Right sub -> applySubst sub t1 === applySubst sub t2
        Left _ -> property True

    prop "returns an idempotent substitution" $
      forAllTerms2 $ \t1 t2 -> case unify t1 t2 of
        Right sub -> (applySubst sub <$> sub) === sub
        Left _ -> property True

    prop "succeeds or fails regardless of the order of the terms" $
      forAllTerms2 $ \t1 t2 -> isRight (unify t1 t2) === isRight (unify t2 t1)

    prop "returns a most general unifier" $
      -- Any idempotent substitution unifies a term with its own instance,
      -- so it must factor through the most general unifier.
      forAll genIdempotentSubst $ \theta -> forAllTerm $ \t ->
        let t' = applySubst theta t
         in case unify t t' of
              Left err -> counterexample (show err) False
              Right sub ->
                conjoin [applySubst theta (applySubst sub u) === applySubst theta u | u <- [t, t']]

  describe "unifyAll" $ do
    it "returns the empty substitution for no equations" $ do
      unifyAll [] `shouldBe` Right Map.empty

    it "solves a system of equations" $ do
      unifyAll [Equation x (f [y]), Equation y a]
        `shouldBe` Right (Map.fromList [("X", f [a]), ("Y", a)])
      unifyAll [Equation (g [x]) (g [y]), Equation y (f [z]), Equation z b]
        `shouldBe` Right (Map.fromList [("X", f [b]), ("Y", f [b]), ("Z", b)])

    it "fails if any equation fails" $ do
      unifyAll [Equation x a, Equation x b] `shouldBe` Left (Clash "a" "b")
      unifyAll [Equation x (f [y]), Equation y (g [x])]
        `shouldBe` Left (OccursCheck "Y" (g [f [y]]))
