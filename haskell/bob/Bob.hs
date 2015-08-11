{-# LANGUAGE UnicodeSyntax #-}

module Bob (responseFor) where

import Prelude.Unicode
import Data.Char
import Text.Regex.PCRE
import Control.Applicative


responseFor ∷ String → String
responseFor input
    | isYelled  input = reponseYelled
    | isAsked   input = responseAsked
    | isGarbage input = responseGarbage
    | otherwise       = responseOtherwise
    where
      reponseYelled     = "Whoa, chill out!"
      responseAsked     = "Sure."
      responseGarbage   = "Fine. Be that way!"
      responseOtherwise = "Whatever."

allCaps ∷ String → Bool
allCaps =  (=~ "^([[:upper:]]+|\\W+|\\s+)+\\W*$")

--(∨∨) = liftA2 (∨)

isYelled, isAsked, isGarbage ∷ String → Bool
isYelled  = allCaps
isAsked   = (`endsWith` '?')
isGarbage = not ∘ any isAlpha


endsWith ∷ String → Char → Bool
endsWith s c = last s ≡ c
