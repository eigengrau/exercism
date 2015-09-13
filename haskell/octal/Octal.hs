{-# LANGUAGE UnicodeSyntax #-}

module Octal (showOct, readOct) where

import Prelude.Unicode


showOct ∷ Integral α ⇒ α → String
showOct = undefined


readOct ∷ Integral α ⇒ String → α
readOct = undefined


ord = undefined

    where
      paired = zip ['0'..'7'] [0..7]
