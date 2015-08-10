{-# LANGUAGE UnicodeSyntax #-}

module Grains (square, total) where

import Prelude.Unicode.SR


square ∷ Integral α ⇒ α → α
square 1 = 1
square n
    | n > 0     = square (pred n) × 2
    | otherwise = undefined


total ∷ Integral α ⇒ α
total = sum $ map square [1..64]
