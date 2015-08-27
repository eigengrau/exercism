{-# LANGUAGE UnicodeSyntax #-}

module Squares (sumOfSquares, squareOfSums, difference) where

import Prelude.Unicode.SR


sumOfSquares ∷ Integral α ⇒ α → α
sumOfSquares x = sum $ map (↑2) [1..x]


squareOfSums ∷ Integral α ⇒ α → α
squareOfSums x = sum [1..x] ↑ 2


difference ∷ Integral α ⇒ α → α
difference x = squareOfSums x - sumOfSquares x
