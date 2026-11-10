-- | QuickCheck generators for terms and substitutions.
module Support.Generators
  ( -- * Terms
    genTerm
  , genTermOver
  , shrinkTerm
  , forAllTerm
  , forAllTerms2
  , forAllTermList

    -- * Substitutions
  , genSubst
  , genIdempotentSubst
  , genRenaming
  ) where

import Data.List (inits, tails)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Test.QuickCheck hiding (Fun)

import Dovetail

-- | Small pools of names, so that generated terms often share structure.
varNames :: [Name]
varNames = ["X", "Y", "Z", "W"]

functors :: [(Name, Int)]
functors = [("f", 1), ("f", 2), ("g", 2), ("h", 3)]

-- | A term over the default variable names.
genTerm :: Gen Term
genTerm = genTermOver varNames

-- | A term whose variables are drawn from the given names.
genTermOver :: [Name] -> Gen Term
genTermOver names = sized go
  where
    go n
      | n <= 1 = leaf
      | otherwise = frequency [(2, leaf), (3, compound n)]

    leaf =
      frequency
        [ (3, Var <$> elements names)
        , (2, atom <$> elements ["a", "b"])
        , (1, atom . show <$> (arbitrary :: Gen Integer))
        ]

    compound n = do
      (f, k) <- elements functors
      Fun f <$> vectorOf k (go (n `div` (k + 1)))

-- | Shrink to a subterm, or shrink one argument in place.
shrinkTerm :: Term -> [Term]
shrinkTerm (Var _) = []
shrinkTerm (Fun f ts) =
  ts ++ [Fun f (pre ++ t' : post) | (pre, t : post) <- zip (inits ts) (tails ts), t' <- shrinkTerm t]

-- | Quantify over a term, with shrinking.
forAllTerm :: Testable prop => (Term -> prop) -> Property
forAllTerm = forAllShrink genTerm shrinkTerm

-- | Quantify over two terms, with shrinking.
forAllTerms2 :: Testable prop => (Term -> Term -> prop) -> Property
forAllTerms2 p = forAllTerm $ \t1 -> forAllTerm $ \t2 -> p t1 t2

-- | Quantify over one or more terms, with shrinking.
forAllTermList :: Testable prop => (NonEmpty Term -> prop) -> Property
forAllTermList = forAllShrink (NonEmpty.fromList <$> listOf1 genTerm) shrinkTerms
  where
    shrinkTerms (t :| ts) =
      [t' :| ts | t' <- shrinkTerm t] ++ [t :| ts' | ts' <- shrinkList shrinkTerm ts]

-- | Any substitution over the default variable names.
genSubst :: Gen Subst
genSubst = Map.fromList <$> listOf ((,) <$> elements varNames <*> scale (`div` 4) genTerm)

-- | A substitution whose bound variables never occur in its range.
genIdempotentSubst :: Gen Subst
genIdempotentSubst =
  Map.fromList <$> listOf ((,) <$> elements ["X", "Y"] <*> scale (`div` 4) (genTermOver ["Z", "W"]))

-- | A bijective renaming of the default variable names to fresh ones.
genRenaming :: Gen Subst
genRenaming = do
  fresh <- shuffle ["A", "B", "C", "D"]
  pure (Map.fromList (zip varNames (Var <$> fresh)))
