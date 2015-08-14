{-# LANGUAGE UnicodeSyntax #-}

module Roman (numerals) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Unicode
import           Data.Char
import qualified Data.List             as List
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe
import           Prelude.Unicode
import           Prelude.Unicode.SR



numerals ∷ Int → String
numerals x = positionalValues x ≫= toRomanNumeral 


toRomanNumeral ∷ Int → String
toRomanNumeral x
  -- If we have digit for this value, just use it.
  | x ∈ romanValues = fromJust $ Map.lookup x arabicDigits
  -- Else, try aiming for a higher value and build a subtractive form.
  | otherwise = fromMaybe (additiveForm x) (subtractiveForm x)


subtractiveForm ∷ Int → Maybe String
subtractiveForm x = remainDigit ↑⧺ largerDigit
  where
    largerValue   = fromMaybe maxRomanValue $ List.find (>x) (reverse romanValues)
    largerDigit   = Map.lookup largerValue arabicDigits
    remainValue   = largerValue - x
    -- Only the next two lesser digits may be prepended subtractively.
    remainDigit   = guard (remainValue ∈ precedingValues largerValue) ≫
                      Map.lookup remainValue arabicDigits
    maxRomanValue = head romanValues
    (↑⧺) = liftA2 (⧺)


precedingValues ∷ Int → [Int]
precedingValues x = take 2 $ dropWhile (≥x) romanValues


additiveForm ∷ Int → String
additiveForm 0 = ""
additiveForm x = smallerDigit ⧺ remainDigits
  where
    smallerValue  = fromMaybe 1 $ List.find (≤x) romanValues
    smallerDigit  = fromJust $ Map.lookup smallerValue arabicDigits
    remainValue   = x - smallerValue
    remainDigits   = additiveForm remainValue


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


-- | The descending list of numbers which have atomic representations
-- in the roman numeral system.
romanValues ∷ [Int]
romanValues = reverse ∘ List.sort $ Map.elems romanDigits


-- | Mapping from those numbers that have an atomic representation in
-- the roman numeral system, to their digit representations.
arabicDigits ∷ Map Int String
arabicDigits = Map.fromList [
    (arabic, roman) | (roman, arabic) ← Map.toList romanDigits
  ]


-- | Return the positional value of each digit in the integer. E. g.,
-- 123 ↦ [100, 20, 3].
positionalValues ∷ Int → [Int]
positionalValues x = zipWith (×) digits multipliers
  where

    digits = map toNum (show x)

    multipliers  = reverse $ take (length digits) multipliers'
    multipliers' = 1 : map (×10) multipliers'

    toNum c
      | result ≥ 0 ∧ result ≤ 9 = result
      | otherwise               = undefined
      where result = ord c - 48
