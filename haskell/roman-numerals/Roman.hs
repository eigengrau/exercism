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


-- | Atomic digits in the roman numeral system and their associated
-- values.
romanDigits ∷ Map String Int
romanDigits = Map.fromList [
    ("I",    1),
    ("V",    5),
    ("X",   10),
    ("L",   50),
    ("C",  100),
    ("D",  500),
    ("M", 1000)
  ]


-- | Mapping from those numbers that have an atomic representation in
-- the roman numeral system, to their digit representations.
arabicDigits = Map.fromList [
    (arabic, roman) | (roman, arabic) ← Map.toList romanDigits
  ]


-- | When a subtractive representation is longer than
-- concatenating the atomic form, use the concatenated form.
fallbackCompose ∷ Int → [String]
fallbackCompose x =
     zipWith (\x y → maybe x (shortest x) y) (characterize $ valuate x) (prefixCompose x)
  where
    shortest s₁ s₂
      | length s₁ < length s₂ = s₁
      | otherwise             = s₂

-- | Roman numerals can be composed in a subtractive mode, where a
-- lesser digit is prefix to a larger one. However, it seems that only
-- one digit can be prefixed this way.
prefixCompose ∷ Int → [Maybe String]
prefixCompose x =
  zipWith whenLegal
    (characterize $ diffs x)
    (characterize ∘ map nearest $ valuate x)

    where
      whenLegal prefix suffix
        | length prefix > 1 = Nothing
        | otherwise         = Just (prefix ⧺ suffix)

characterize ∷ [Int] → [String]
characterize = map characterize'
  where
    characterize' 0 = ""
    characterize' target
      | target ∈ candidates = fromJust $ Map.lookup target arabicDigits
      | otherwise           = smallerRoman ⧺ concat (characterize [remain])
      where
        -- First, we look for a smaller digit out of which we can
        -- compose x.
        smaller      = fromJust $ List.find (<target) candidates
        smallerRoman = fromJust $ Map.lookup smaller arabicDigits

        -- Since the digit was smaller, we must add other digits.
        remain       = target - smaller

        forced       = undefined --replicate smallerRoman


-- | Try mapping each digit of the number to the closest
-- equal-or-bigger roman digit, then return how much larger than the
-- goal each roman digit is.
diffs ∷ Int → [Int]
diffs x = zipWith subtract' (map nearest vals) vals
  where
    subtract' 0 _ = 0
    subtract' x y = abs $ x - y
    vals = valuate x


-- | Convert a integer into a roman numeric expression.
numerals ∷ Int → String
numerals x = concat (fallbackCompose x)


-- | The descending list of numbers which have atomic representations
-- in the roman numeral system.
candidates ∷ [Int]
candidates = reverse ∘ List.sort $ Map.elems romanDigits


-- | Given some number, find the nearest equal or larger number that has an
-- atomic representation in the roman numeral system.
nearest ∷ Int → Int
nearest 0 = 0
nearest x
  | x `elem` candidates = x
  | otherwise           =
      let f = filter (>x) candidates
      in if null f
         then head candidates
         else last f


-- | Split the integer into digits, and return the numeric value of
-- each digit. E. g., 123 ↦ [100, 20, 3].
valuate ∷ Int → [Int]
valuate x = zipWith (×) digits (significants l)
  where
    l = length (show x)
    digits = map toNum (show x)


-- | `significants n` gives the list of value multipliers for each position in a digit
-- of length n.
significants ∷ Int → [Int]
significants 1 = [1]
significants n = head next × 10 : next
  where
    next = significants (pred n)


-- | Convert a number character into an integer.
toNum ∷ Char → Int
toNum c
  | c ∈ ['0'..'9'] = ord c - 48
  | otherwise      = undefined


-- | Convert a an integer into the corresponding character
-- representation.
toChar ∷ Int → Char
toChar n
  | n ≥ 0 ∧ n ≤ 9 = chr (n + 48)
  | otherwise     = undefined
