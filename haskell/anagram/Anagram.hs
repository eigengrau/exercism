{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module Anagram (anagramsFor) where

import           Data.Char
import           Data.List
import           Prelude.Unicode

anagramsFor ∷ String → [String] → [String]
anagramsFor word = filter (`anagramOf` word)

anagramOf ∷ String → String → Bool
anagramOf (map toLower → w₁) (map toLower → w₂) =
  w₁ ≢ w₂ ∧ sort w₁ ≡ sort w₂
