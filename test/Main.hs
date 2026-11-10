module Main (main) where

import Test.Hspec

import AntiUnifySpec qualified
import MatchSpec qualified
import ParserSpec qualified
import PrettySpec qualified
import SubstSpec qualified
import UnifySpec qualified

main :: IO ()
main = hspec $ do
  describe "Substitutions" SubstSpec.spec
  describe "Unification" UnifySpec.spec
  describe "Matching" MatchSpec.spec
  describe "Anti-unification" AntiUnifySpec.spec
  describe "Parser" ParserSpec.spec
  describe "Pretty" PrettySpec.spec
