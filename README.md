# dovetail-unify

[![CI](https://github.com/joaquin-rossi/dovetail-unify/actions/workflows/ci.yml/badge.svg)](https://github.com/joaquin-rossi/dovetail-unify/actions/workflows/ci.yml)

Haskell library for syntactic unification of first-order terms.

## Example

```haskell
import Dovetail

lhs = Fun "f" [atom "a", Var "Y"]
rhs = Fun "f" [Var "X", atom "b"]

unify lhs rhs
-- Right (fromList [("X",Fun "a" []),("Y",Fun "b" [])])
```

`antiUnify` computes the least general generalization of two terms, along
with the substitutions that recover each of them:

```haskell
antiUnify (Fun "f" [atom "a", atom "a"]) (Fun "f" [atom "b", atom "b"])
-- (Fun "f" [Var "G1",Var "G1"],fromList [("G1",Fun "a" [])],fromList [("G1",Fun "b" [])])
```

Terms and equations can also be parsed from Prolog-style syntax:

```haskell
import Dovetail.Parser

parseTerm "f(X, g(a))"
-- Right (Fun "f" [Var "X",Fun "g" [Fun "a" []]])

parseEquation "f(a, Y) = f(X, b)"
-- Right (Equation (Fun "f" [Fun "a" [],Var "Y"]) (Fun "f" [Var "X",Fun "b" []]))

parseEquations "X = f(Y), Y = a"
-- Right [Equation (Var "X") (Fun "f" [Var "Y"]),Equation (Var "Y") (Fun "a" [])]
```
