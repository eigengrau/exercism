{-# LANGUAGE UnicodeSyntax #-}

module Accumulate (accumulate) where

accumulate ∷ (α → β) → [α] → [β]
accumulate _ []     = []
accumulate f (x:xs) = f x : accumulate f xs
