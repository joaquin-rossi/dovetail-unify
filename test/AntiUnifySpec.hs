module AntiUnifySpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (Fun)

import Dovetail
import Support.Generators
import Support.Terms

spec :: Spec
spec = do
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

    prop "recovers any two terms" $
      forAllTerms2 $ \t1 t2 ->
        let (gen, sub1, sub2) = antiUnify t1 t2
         in (applySubst sub1 gen, applySubst sub2 gen) === (t1, t2)

    prop "generalizes a term with itself to itself" $
      forAllTerm $ \t -> antiUnify t t === (t, Map.empty, Map.empty)

    prop "is subsumed by every common generalization" $
      forAll genSubst $ \sub1 -> forAll genSubst $ \sub2 -> forAllTerm $ \h ->
        let (gen, _, _) = antiUnify (applySubst sub1 h) (applySubst sub2 h)
         in counterexample (show gen) (subsumes h gen)

  describe "antiUnifyAll" $ do
    it "generalizes a single term to itself" $ do
      antiUnifyAll (f [x, a] :| []) `shouldBe` (f [x, a], Map.empty :| [])

    it "generalizes many terms at once" $ do
      antiUnifyAll (f [a, b] :| [f [a, c], f [a, a]])
        `shouldBe`
          ( f [a, Var "G1"]
          , Map.fromList [("G1", b)] :| [Map.fromList [("G1", c)], Map.fromList [("G1", a)]]
          )

    it "reuses the variable for a repeated column" $ do
      let (gen, _) = antiUnifyAll (f [a, a] :| [f [b, b], f [c, c]])
      gen `shouldBe` f [Var "G1", Var "G1"]

    prop "recovers every term" $
      forAllTermList $ \ts ->
        let (gen, subs) = antiUnifyAll ts
         in (flip applySubst gen <$> subs) === ts

    prop "agrees with antiUnify on two terms" $
      forAllTerms2 $ \t1 t2 ->
        let (gen, sub1, sub2) = antiUnify t1 t2
         in antiUnifyAll (t1 :| [t2]) === (gen, sub1 :| [sub2])
