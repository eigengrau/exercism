{-# LANGUAGE UnicodeSyntax #-}

module LeapYear (isLeapYear) where

import Prelude.Unicode
import Control.Applicative


type Year = Integer


(∧∧) ∷ (α → Bool) → (α → Bool) → α → Bool
(∧∧) = liftA2 (∧)
infixr 2 ∧∧


(∨∨) ∷ (α → Bool) → (α → Bool) → α → Bool
(∨∨) = liftA2 (∨)
infixr 3 ∨∨


divides ∷ Integral α ⇒ α → α → Bool
n `divides` m = m `mod` n ≡ 0


isLeapYear ∷ Year → Bool
isLeapYear = baseCriterion ∧∧ (not ∘ isExcluded ∨∨ exclusionRelaxed)
  where
    baseCriterion    = (4   `divides`)
    isExcluded       = (100 `divides`)
    exclusionRelaxed = (400 `divides`)
