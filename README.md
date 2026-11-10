# dovetail-unify

Haskell library for syntactic unification of first-order terms.

## Example

```haskell
import Dovetail

lhs = TFun "f" [atom "a", TVar "Y"]
rhs = TFun "f" [TVar "X", atom "b"]

unify (Equation lhs rhs)
-- Right (fromList [("X",TFun "a" []),("Y",TFun "b" [])])
```

`unify` returns either a substitution (`Subst`) or a `UnifyError`
(`Clash` or `OccursCheck`).

Terms and equations can also be parsed from Prolog-style syntax:

```haskell
import Dovetail.Parser

parseTerm "f(X, g(a))"
-- Right (TFun "f" [TVar "X",TFun "g" [TFun "a" []]])

parseEquation "f(a, Y) = f(X, b)"
-- Right (Equation (TFun "f" [TFun "a" [],TVar "Y"]) (TFun "f" [TVar "X",TFun "b" []]))
```
