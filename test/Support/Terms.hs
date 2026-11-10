-- | Short names for the terms used throughout the examples.
module Support.Terms
  ( a
  , b
  , c
  , f
  , g
  , x
  , y
  , z
  ) where

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
