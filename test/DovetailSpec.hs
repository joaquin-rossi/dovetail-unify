module DovetailSpec (spec) where

import Test.Hspec
import Data.Map.Strict qualified as Map

import Dovetail

a, b, c :: Term
a = atom "a"
b = atom "b"
c = atom "c"

f, g :: [Term] -> Term
f = Fun "f"
g = Fun "g"

x, y, z :: Term
x = Var "X"
y = Var "Y"
z = Var "Z"

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
      unify (f [a]) (f [a, b]) `shouldBe` Left (Clash "f" "f")
      unify a (f [a]) `shouldBe` Left (Clash "a" "f")

    it "fails the occurs check" $ do
      unify x (f [x]) `shouldBe` Left (OccursCheck "X" (f [x]))
      unify (g [x]) x `shouldBe` Left (OccursCheck "X" (g [x]))

    it "fails the occurs check through other bindings" $ do
      unify (f [x, y]) (f [y, g [x]])
        `shouldBe` Left (OccursCheck "Y" (g [y]))

  describe "antiUnify" $ do
    let recovers t1 t2 =
          let (gen, sub1, sub2) = antiUnify t1 t2
           in (applySubst sub1 gen, applySubst sub2 gen) `shouldBe` (t1, t2)

    it "returns equal terms unchanged" $ do
      antiUnify (f [a, x]) (f [a, x])
        `shouldBe` (f [a, x], Map.empty, Map.empty)

    it "generalizes differing subterms" $ do
      antiUnify (f [a, b]) (f [c, b])
        `shouldBe` (f [Var "G1", b], Map.fromList [("G1", a)], Map.fromList [("G1", c)])

    it "reuses the variable for a repeated pair" $ do
      antiUnify (f [a, a]) (f [b, b])
        `shouldBe` (f [Var "G1", Var "G1"], Map.fromList [("G1", a)], Map.fromList [("G1", b)])

    it "uses distinct variables for distinct pairs" $ do
      let (gen, _, _) = antiUnify (f [a, b]) (f [b, a])
      gen `shouldBe` f [Var "G1", Var "G2"]

    it "generalizes clashing functors and arities to a variable" $ do
      let (gen1, _, _) = antiUnify (f [a]) (g [a])
          (gen2, _, _) = antiUnify (f [a]) (f [a, b])
      gen1 `shouldBe` Var "G1"
      gen2 `shouldBe` Var "G1"

    it "avoids variable names already in use" $ do
      let (gen, _, _) = antiUnify (f [Var "G1", a]) (f [Var "G1", b])
      gen `shouldBe` f [Var "G1", Var "G2"]

    it "recovers both terms" $ do
      recovers (f [a, g [x, b]]) (f [c, g [y, b]])
      recovers (f [x, x]) (f [a, b])
      recovers a (f [a])

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
