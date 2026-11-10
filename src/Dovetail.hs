module Dovetail where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type Name = String

data Term =
    Fun Name [Term]
  | Var Name
  deriving (Eq, Show)

atom :: Name -> Term
atom s = Fun s []

type Subst = Map Name Term

applySubst :: Subst -> Term -> Term
applySubst s = go
  where
    go (Fun n ts) = Fun n (go <$> ts)
    go t@(Var n)  = Map.findWithDefault t n s

extendSubst :: Name -> Term -> Subst -> Subst
extendSubst n t s = 
  Map.insert n t $
    applySubst (Map.singleton n t) <$> s

data Equation = Equation !Term !Term deriving (Eq, Show)

data UnifyError =
    Clash Name Name
  | OccursCheck Name Term
  deriving (Eq, Show)

occursVar :: Name -> Term -> Bool
occursVar n (Fun _ ts) = any (occursVar n) ts
occursVar n (Var m) = n == m

unify :: Equation -> Either UnifyError Subst
unify e = solve Map.empty [e]

solve :: Subst -> [Equation] -> Either UnifyError Subst
solve s [] = Right s
solve s (e:rest) =
  case e of
    -- Delete: X = X
    Equation (Var n) (Var m) | n == m -> solve s rest
    -- Eliminate: X = t
    Equation (Var n) t -> eliminate n t
    -- Orient: t = X
    Equation t (Var n) -> eliminate n t
    -- Decompose or Clash
    Equation (Fun n us) (Fun m vs)
      | n == m && length us == length vs -> solve s (zipWith Equation us vs ++ rest)
      | otherwise -> Left (Clash n m)
  where
    eliminate n t
      | occursVar n t = Left (OccursCheck n t)
      | otherwise = solve (extendSubst n t s) (both (applySubst (Map.singleton n t)) <$> rest)
        where both f (Equation a b) = Equation (f a) (f b)
