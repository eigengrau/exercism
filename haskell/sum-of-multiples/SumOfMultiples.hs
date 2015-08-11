{-# LANGUAGE UnicodeSyntax #-}

module SumOfMultiples (sumOfMultiples, sumOfMultiplesDefault) where

import           Control.Monad
import qualified Data.Foldable   as Fold
import           Data.Maybe
import           Data.Monoid
import           Prelude.Unicode


sumOfMultiples ∷ Integral α ⇒ [α] → α → α
sumOfMultiples factors n = sum (multiplesOf factors n)


sumOfMultiplesDefault ∷ Integral α ⇒ α → α
sumOfMultiplesDefault = sumOfMultiples [3, 5]


multiplesOf ∷ Integral α ⇒ [α] → α → [α]
multiplesOf factors n = mapMaybe checkNum candidates

    where
      checkNum n = getFirst ∘ Fold.fold $ map ($n) conditions
      conditions = map (\factor n → First (n `whenMultipleOf` factor)) factors
      candidates = [0..pred n]


isMultipleOf ∷ Integral α ⇒ α → α → Bool
n `isMultipleOf` m = n `mod` m ≡ 0


whenMultipleOf ∷ Integral α ⇒ α → α → Maybe α
n `whenMultipleOf` m = mfilter (`isMultipleOf` m) (pure n)
