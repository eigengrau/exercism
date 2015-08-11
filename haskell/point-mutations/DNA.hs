{-# LANGUAGE UnicodeSyntax #-}

module DNA (hammingDistance) where

import Prelude.Unicode


hammingDistance ∷ String → String → Int
hammingDistance s₁ s₂
  | length s₁ ≢ length s₂ = undefined
  | otherwise             = hammingDistanceʹ s₁ s₂


hammingDistanceʹ ∷ String → String → Int
hammingDistanceʹ s₁ s₂ = length ∘ filter (≡False) $ zipWith (≡) s₁ s₂
