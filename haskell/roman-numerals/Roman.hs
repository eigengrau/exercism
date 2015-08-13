{-# LANGUAGE UnicodeSyntax #-}

module Roman (numerals) where

import Prelude.Unicode.SR
import qualified Data.List as List
import Prelude.Unicode
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import Data.Char

romanDigits = Map.fromList [
    ("I",    1),
    -- ("II",   2),
    -- ("III",  3),
    -- ("IV",   4),
    ("V",    5),
    -- ("VI",   6),
    -- ("VII",  7),
    -- ("VIII", 8),
    -- ("IX",   9),
    ("X",   10),
    ("L",   50),
    ("C",  100),
    ("D",  500),
    ("M", 1000)
  ]

arabicDigits = Map.fromList [
    (arabic, roman) | (roman, arabic) ← Map.toList romanDigits
  ]

make x = zipWith (⧺) (characterize $ diffs x) (characterize ∘ map nearest $ valuate x)

characterize = map characterize'
  where
    characterize' 0 = ""
    characterize' x
      | x ∈ candidates = fromJust $ Map.lookup x arabicDigits
      | otherwise = 
      where
        alternative = concat $ replicate required smallerRoman
        forced = replicate smallerRoman
        smaller = fromJust $ List.find (<x) candidates
        smallerRoman = fromJust $ Map.lookup smaller arabicDigits
        required = x `div` smaller

diffs x = zipWith (-) (map nearest vals) vals
  where
    vals = valuate x

numerals = undefined

candidates = reverse ∘ List.sort $ Map.elems romanDigits

nearest ∷ Int → Int
nearest x
  | x `elem` candidates = x
  | otherwise = last $ filter (≥x) candidates

valuate x = zipWith (×) digits (significants l)
  where
    l = length (show x)
    digits = map toNum (show x)

significants 1 = [1]
significants n = head next × 10 : next
  where
    next = significants (pred n)

toNum ∷ Char → Int
toNum = (subtract 48) ∘ ord

toChar ∷ Int → Char
toChar = chr ∘ (+48)
