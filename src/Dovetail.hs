-- | Syntactic unification and anti-unification of first-order terms.
module Dovetail
  ( -- * Terms
    Name
  , Term (..)
  , atom
  , vars
  , occursVar

    -- * Substitutions
  , Subst
  , applySubst
  , composeSubst
  , extendSubst

    -- * Unification
  , Equation (..)
  , UnifyError (..)
  , unify
  , unifyAll

    -- * Anti-unification
  , antiUnify
  ) where

import Data.List (mapAccumL)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | Name of a functor or a variable.
type Name = String

-- | A first-order term.
data Term =
    -- | A functor applied to its arguments.
    Fun Name [Term]
    -- | A variable.
  | Var Name
  deriving (Eq, Ord, Show)

-- | A functor with no arguments.
atom :: Name -> Term
atom s = Fun s []

-- | The variables occurring in a term.
vars :: Term -> Set Name
vars (Fun _ ts) = foldMap vars ts
vars (Var n) = Set.singleton n

-- | A mapping from variables to the terms that replace them.
type Subst = Map Name Term

-- | Replace every bound variable in a term, in a single pass.
applySubst :: Subst -> Term -> Term
applySubst sub = go
  where
    go (Fun n ts) = Fun n (go <$> ts)
    go t@(Var n)  = Map.findWithDefault t n sub

-- | Composition of substitutions, like function composition: applying
-- @composeSubst sub2 sub1@ is the same as applying @sub1@ and then @sub2@.
--
-- Bindings that become trivial, like @X ↦ X@, are dropped.
composeSubst :: Subst -> Subst -> Subst
composeSubst sub2 sub1 =
  Map.filterWithKey (\n t -> t /= Var n) $
    (applySubst sub2 <$> sub1) `Map.union` sub2

-- | Bind a variable to a term, applying the new binding to the existing ones.
extendSubst :: Name -> Term -> Subst -> Subst
extendSubst n t sub =
  Map.insert n t $
    applySubst (Map.singleton n t) <$> sub

-- | An equation between two terms, @lhs = rhs@.
data Equation = Equation !Term !Term deriving (Eq, Show)

-- | Why two terms cannot be unified.
data UnifyError =
    -- | Two functors differ in name or arity.
    Clash Name Name
    -- | A variable would have to be bound to a term that contains it.
  | OccursCheck Name Term
  deriving (Eq, Show)

-- | Whether a variable occurs in a term.
occursVar :: Name -> Term -> Bool
occursVar n (Fun _ ts) = any (occursVar n) ts
occursVar n (Var m) = n == m

-- | Most general unifier of two terms.
--
-- Returns @sub@ such that @applySubst sub t1 == applySubst sub t2@, or a
-- 'Clash' when two functors (or their arities) differ, or an 'OccursCheck'
-- when a variable would have to contain itself.
unify :: Term -> Term -> Either UnifyError Subst
unify t1 t2 = unifyAll [Equation t1 t2]

-- | Most general unifier of a system of equations: a single substitution
-- that makes both sides of every equation equal.
unifyAll :: [Equation] -> Either UnifyError Subst
unifyAll [] = Right Map.empty
unifyAll (eq : rest) =
  case eq of
    -- Delete: X = X
    Equation (Var n) (Var m) | n == m -> unifyAll rest
    -- Eliminate: X = t
    Equation (Var n) t -> eliminate n t
    -- Orient: t = X
    Equation t (Var n) -> eliminate n t
    -- Decompose or Clash
    Equation (Fun n us) (Fun m vs)
      | n == m && length us == length vs -> unifyAll (zipWith Equation us vs ++ rest)
      | otherwise -> Left (Clash n m)
  where
    -- Solve the remaining equations with X replaced by t, then bind X to
    -- t under that solution.
    eliminate :: Name -> Term -> Either UnifyError Subst
    eliminate n t
      | occursVar n t = Left (OccursCheck n t)
      | otherwise = do
          sub <- unifyAll (both (applySubst (Map.singleton n t)) <$> rest)
          pure (Map.insert n (applySubst sub t) sub)

    both :: (Term -> Term) -> Equation -> Equation
    both f (Equation a b) = Equation (f a) (f b)

-- | Least general generalization of two terms.
--
-- Returns @(g, sub1, sub2)@ such that @applySubst sub1 g == t1@ and
-- @applySubst sub2 g == t2@. Fresh variables are named @G1@, @G2@, ...,
-- skipping names already used in either term.
antiUnify :: Term -> Term -> (Term, Subst, Subst)
antiUnify t1 t2 = (g, side fst, side snd)
  where
    ((pairs, _), g) = go (Map.empty, 1) t1 t2

    side pick = Map.fromList [(v, pick p) | (p, v) <- Map.toList pairs]

    used = vars t1 <> vars t2
    name i = 'G' : show (i :: Int)

    go st a b
      | a == b = (st, a)
    go st (Fun f as) (Fun h bs)
      | f == h && length as == length bs =
          Fun f <$> mapAccumL (\st' (a, b) -> go st' a b) st (zip as bs)
    go st@(ps, next) a b =
      case Map.lookup (a, b) ps of
        Just v -> (st, Var v)
        Nothing ->
          let i = until ((`Set.notMember` used) . name) (+ 1) next
              v = name i
           in ((Map.insert (a, b) v ps, i + 1), Var v)
