module ParserSpec (spec) where

import Data.Either (isLeft)
import Test.Hspec

import Dovetail
import Dovetail.Parser

spec :: Spec
spec = do
  describe "parseTerm" $ do
    it "parses variables" $ do
      parseTerm "X" `shouldBe` Right (TVar "X")
      parseTerm "Foo_1" `shouldBe` Right (TVar "Foo_1")

    it "parses atoms" $ do
      parseTerm "a" `shouldBe` Right (atom "a")
      parseTerm "nil" `shouldBe` Right (atom "nil")
      parseTerm "a_1" `shouldBe` Right (atom "a_1")

    it "parses integers as atoms" $ do
      parseTerm "42" `shouldBe` Right (atom "42")
      parseTerm "007" `shouldBe` Right (atom "7")
      parseTerm "-7" `shouldBe` Right (atom "-7")
      parseTerm "-0" `shouldBe` Right (atom "0")
      parseTerm "f(1, X)" `shouldBe` Right (TFun "f" [atom "1", TVar "X"])

    it "parses functors with arguments" $ do
      parseTerm "f(X, g(a), b)"
        `shouldBe` Right (TFun "f" [TVar "X", TFun "g" [atom "a"], atom "b"])

    it "ignores whitespace between tokens" $ do
      parseTerm "  f( X ,  a )  " `shouldBe` Right (TFun "f" [TVar "X", atom "a"])
      parseTerm "f (a)" `shouldBe` parseTerm "f(a)"

    it "rejects malformed input" $ do
      parseTerm "" `shouldSatisfy` isLeft
      parseTerm "f()" `shouldSatisfy` isLeft
      parseTerm "f(a" `shouldSatisfy` isLeft
      parseTerm "f(a,)" `shouldSatisfy` isLeft
      parseTerm "X(a)" `shouldSatisfy` isLeft
      parseTerm "_" `shouldSatisfy` isLeft
      parseTerm "42(a)" `shouldSatisfy` isLeft
      parseTerm "1a" `shouldSatisfy` isLeft
      parseTerm "-" `shouldSatisfy` isLeft
      parseTerm "- 1" `shouldSatisfy` isLeft
      parseTerm "--1" `shouldSatisfy` isLeft
      parseTerm "-a" `shouldSatisfy` isLeft
      parseTerm "a = b" `shouldSatisfy` isLeft

    it "reports the position of the error" $ do
      either show (const "") (parseTerm "f(a, )") `shouldContain` "column 6"

  describe "parseEquation" $ do
    it "parses equations" $ do
      parseEquation "f(X, b) = f(a, Y)"
        `shouldBe` Right (Equation (TFun "f" [TVar "X", atom "b"]) (TFun "f" [atom "a", TVar "Y"]))
      parseEquation "X=a" `shouldBe` Right (Equation (TVar "X") (atom "a"))

    it "rejects malformed equations" $ do
      parseEquation "a" `shouldSatisfy` isLeft
      parseEquation "a =" `shouldSatisfy` isLeft
      parseEquation "= a" `shouldSatisfy` isLeft
      parseEquation "a = b = c" `shouldSatisfy` isLeft

    it "works with unify" $ do
      let result = do
            e@(Equation lhs _) <- either (Left . show) Right (parseEquation "f(a, Y) = f(X, b)")
            s <- either (Left . show) Right (unify e)
            pure (applySubst s lhs)
      result `shouldBe` Right (TFun "f" [atom "a", atom "b"])
