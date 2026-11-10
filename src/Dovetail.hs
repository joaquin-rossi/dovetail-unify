module Dovetail where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type Name = String

data Term =
    TFun Name [Term]
  | TVar Name
  deriving (Eq, Show)

atom :: Name -> Term
atom s = TFun s []

type Subst = Map Name Term

applySubst :: Subst -> Term -> Term
applySubst s = go
  where
    go (TFun n ts) = TFun n (go <$> ts)
    go t@(TVar n)  = Map.findWithDefault t n s

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
occursVar n (TFun _ ts) = any (occursVar n) ts
occursVar n (TVar m) = n == m

unify :: Equation -> Either UnifyError Subst
unify e = solve Map.empty [e]

solve :: Subst -> [Equation] -> Either UnifyError Subst
solve s [] = Right s
solve s (e:rest) =
  case e of
    -- Delete: X = X
    Equation (TVar n) (TVar m) | n == m -> solve s rest
    -- Eliminate: X = t
    Equation (TVar n) t -> eliminate n t
    -- Orient: t = X
    Equation t (TVar n) -> eliminate n t
    -- Decompose or Clash
    Equation (TFun n us) (TFun m vs)
      | n == m && length us == length vs -> solve s (zipWith Equation us vs ++ rest)
      | otherwise -> Left (Clash n m)
  where
    eliminate n t
      | occursVar n t = Left (OccursCheck n t)
      | otherwise = solve (extendSubst n t s) (both (applySubst (Map.singleton n t)) <$> rest)
        where both f (Equation a b) = Equation (f a) (f b)
