-- | Render terms in the syntax accepted by "Dovetail.Parser".
module Dovetail.Pretty
  ( prettyTerm
  , prettyEquation
  , prettyEquations
  , prettySubst
  ) where

import Data.List (intercalate)
import Data.Map.Strict qualified as Map

import Dovetail (Equation (..), Subst, Term (..))

-- | Render a term: @f(X, g(a))@.
prettyTerm :: Term -> String
prettyTerm (Var n) = n
prettyTerm (Fun f []) = f
prettyTerm (Fun f ts) = f ++ "(" ++ intercalate ", " (prettyTerm <$> ts) ++ ")"

-- | Render an equation: @f(X) = f(a)@.
prettyEquation :: Equation -> String
prettyEquation (Equation l r) = prettyTerm l ++ " = " ++ prettyTerm r

-- | Render comma-separated equations: @X = f(Y), Y = a@.
prettyEquations :: [Equation] -> String
prettyEquations = intercalate ", " . fmap prettyEquation

-- | Render a substitution: @{X ↦ f(a), Y ↦ a}@.
prettySubst :: Subst -> String
prettySubst sub =
  "{" ++ intercalate ", " [n ++ " ↦ " ++ prettyTerm t | (n, t) <- Map.toList sub] ++ "}"
