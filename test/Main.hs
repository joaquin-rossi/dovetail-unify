module Main (main) where

import Test.Hspec

import DovetailSpec qualified
import ParserSpec qualified

main :: IO ()
main = hspec $ do
  describe "Dovetail" DovetailSpec.spec
  describe "Parser" ParserSpec.spec
