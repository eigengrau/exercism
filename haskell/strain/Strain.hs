{-# LANGUAGE UnicodeSyntax #-}

module Strain (keep, discard) where

import Prelude hiding (filter)
import Prelude.Unicode


filter ∷ (α → Bool) → [α] → [α]
filter _ []        = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs


keep ∷ (α → Bool) → [α] → [α]
keep = filter


discard ∷ (α → Bool) → [α] → [α]
discard p = filter (not ∘ p)
