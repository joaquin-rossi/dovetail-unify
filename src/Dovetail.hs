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

    -- * Unification
  , Equation (..)
  , UnifyError (..)
  , unify
  , unifyAll

    -- * Matching
  , match
  , subsumes
  , variant

    -- * Anti-unification
  , antiUnify
  , antiUnifyAll
  ) where

import Data.List (mapAccumL, transpose)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
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

-- | Whether a variable occurs in a term.
occursVar :: Name -> Term -> Bool
occursVar n (Fun _ ts) = any (occursVar n) ts
occursVar n (Var m) = n == m

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
  dropTrivial $ (applySubst sub2 <$> sub1) `Map.union` sub2

dropTrivial :: Subst -> Subst
dropTrivial = Map.filterWithKey (\n t -> t /= Var n)

-- | An equation between two terms, @lhs = rhs@.
data Equation = Equation !Term !Term deriving (Eq, Show)

-- | Why two terms cannot be unified.
data UnifyError =
    -- | Two functors have different names.
    Clash Name Name
    -- | A functor is applied to different numbers of arguments.
  | ArityMismatch Name Int Int
    -- | A variable would have to be bound to a term that contains it.
  | OccursCheck Name Term
  deriving (Eq, Show)

-- | Most general unifier of two terms.
--
-- Returns @sub@ such that @applySubst sub t1 == applySubst sub t2@, or the
-- 'UnifyError' that makes it impossible.
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
    -- Decompose or fail
    Equation (Fun n us) (Fun m vs)
      | n /= m -> Left (Clash n m)
      | length us /= length vs -> Left (ArityMismatch n (length us) (length vs))
      | otherwise -> unifyAll (zipWith Equation us vs ++ rest)
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

-- | One-way unification: a substitution for the variables of the pattern
-- that turns it into the target, if there is one.
--
-- Variables in the target are treated as constants, so
-- @applySubst sub pattern == target@.
match :: Term -> Term -> Maybe Subst
match pat target = dropTrivial <$> go Map.empty [(pat, target)]
  where
    go :: Subst -> [(Term, Term)] -> Maybe Subst
    go sub [] = Just sub
    go sub ((Var n, t) : rest) =
      case Map.lookup n sub of
        Nothing -> go (Map.insert n t sub) rest
        Just t'
          | t' == t -> go sub rest
          | otherwise -> Nothing
    go sub ((Fun f ps, Fun h ts) : rest)
      | f == h && length ps == length ts = go sub (zip ps ts ++ rest)
    go _ _ = Nothing

-- | Whether the first term is at least as general as the second, i.e. the
-- second is an instance of the first.
subsumes :: Term -> Term -> Bool
subsumes general specific = isJust (match general specific)

-- | Whether two terms are equal up to renaming of variables.
variant :: Term -> Term -> Bool
variant t1 t2 = subsumes t1 t2 && subsumes t2 t1

-- | Least general generalization of two terms.
--
-- Returns @(g, sub1, sub2)@ such that @applySubst sub1 g == t1@ and
-- @applySubst sub2 g == t2@. Fresh variables are named @G1@, @G2@, ...,
-- skipping names already used in either term.
antiUnify :: Term -> Term -> (Term, Subst, Subst)
antiUnify t1 t2 = (g, projection ps 0, projection ps 1)
  where
    (g, ps) = generalize [t1, t2]

-- | Least general generalization of one or more terms, along with the
-- substitution that recovers each of them, in order.
antiUnifyAll :: NonEmpty Term -> (Term, NonEmpty Subst)
antiUnifyAll ts@(_ :| rest) = (g, projection ps <$> 0 :| [1 .. length rest])
  where
    (g, ps) = generalize (NonEmpty.toList ts)

-- | Generalize a column of terms. Returns the generalization and, for each
-- fresh variable, the column of terms it stands for.
generalize :: [Term] -> (Term, Map [Term] Name)
generalize terms = (g, ps)
  where
    ((ps, _), g) = go (Map.empty, 1) terms

    used = foldMap vars terms
    name i = 'G' : show (i :: Int)

    go st ts@(t : rest)
      | all (== t) rest = (st, t)
      | Just (f, argss) <- sameFunctor ts =
          Fun f <$> mapAccumL go st (transpose argss)
    go st@(seen, next) ts =
      case Map.lookup ts seen of
        Just v -> (st, Var v)
        Nothing ->
          let i = until ((`Set.notMember` used) . name) (+ 1) next
              v = name i
           in ((Map.insert ts v seen, i + 1), Var v)

    sameFunctor (Fun f as : rest)
      | all matches rest = Just (f, as : [bs | Fun _ bs <- rest])
      where
        matches (Fun h bs) = h == f && length bs == length as
        matches (Var _) = False
    sameFunctor _ = Nothing

-- | The substitution that recovers the term at the given position of the
-- column passed to 'generalize'.
projection :: Map [Term] Name -> Int -> Subst
projection ps i = Map.fromList [(v, ts !! i) | (ts, v) <- Map.toList ps]
