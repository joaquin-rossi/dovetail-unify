# dovetail-unify

Haskell library for syntactic unification of first-order terms.

## Example

```haskell
import Dovetail

lhs = TTerm "f" [atom "1", TVar "y"]
rhs = TTerm "f" [TVar "x", atom "2"]

unify lhs rhs
-- Right (fromList [("x", TTerm "1" []), ("y", TTerm "2" [])])
```

`unify` returns either a substitution (`Subst`) or a `UnifyError`
(`Clash` or `OccursCheck`).
