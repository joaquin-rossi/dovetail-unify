# dovetail-unify

Haskell library for syntactic unification of first-order terms.

## Example

```haskell
import Dovetail

lhs = Fun "f" [atom "a", Var "Y"]
rhs = Fun "f" [Var "X", atom "b"]

unify (Equation lhs rhs)
-- Right (fromList [("X",Fun "a" []),("Y",Fun "b" [])])
```

`unify` returns either a substitution (`Subst`) or a `UnifyError`
(`Clash` or `OccursCheck`).

Terms and equations can also be parsed from Prolog-style syntax:

```haskell
import Dovetail.Parser

parseTerm "f(X, g(a))"
-- Right (Fun "f" [Var "X",Fun "g" [Fun "a" []]])

parseEquation "f(a, Y) = f(X, b)"
-- Right (Equation (Fun "f" [Fun "a" [],Var "Y"]) (Fun "f" [Var "X",Fun "b" []]))
```
