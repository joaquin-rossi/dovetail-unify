module Dovetail where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Dovetail.Util

type Name = String

data Term =
    TTerm Name [Term]
  | TVar Name
  deriving (Eq, Show)

atom :: Name -> Term
atom s = TTerm s []

type Subst = Map Name Term

applySubst :: Subst -> Term -> Term
applySubst s = go
  where
    go (TTerm n ts) = TTerm n (go <$> ts)
    go t@(TVar n)   = Map.findWithDefault t n s

extendSubst :: Name -> Term -> Subst -> Subst
extendSubst n t s = 
  Map.insert n t $
    applySubst (Map.singleton n t) <$> s

data UnifyError =
    Clash Name Name
  | OccursCheck Name Term
  deriving (Eq, Show)

occursVar :: Name -> Term -> Bool
occursVar n (TTerm _ ts) = any (occursVar n) ts
occursVar n (TVar m) = n == m

unify :: Term -> Term -> Either UnifyError Subst
unify t1 t2 = solve Map.empty [(t1, t2)]

solve :: Subst -> [(Term, Term)] -> Either UnifyError Subst
solve s [] = Right s
solve s ((t1,t2):rest) =
  case (t1, t2) of
    -- Delete: X = X
    (TVar n, TVar m) | n == m -> solve s rest
    -- Eliminate: X = t
    (TVar n, t) -> eliminate n t
    -- Orient: t = X
    (t, TVar n) -> eliminate n t
    -- Decompose or Clash
    (TTerm n us, TTerm m vs)
      | n == m && length us == length vs -> solve s (zip us vs ++ rest)
      | otherwise -> Left (Clash n m)
  where
    eliminate n t
      | occursVar n t = Left (OccursCheck n t)
      | otherwise = solve (extendSubst n t s) (both (applySubst (Map.singleton n t)) <$> rest)
