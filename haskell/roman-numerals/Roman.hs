{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}

module Roman (numerals) where

import           Control.Monad
import           Control.Monad.Unicode
import           Data.Char
import           Data.Maybe
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Safe                  hiding (toEnumMay)

import           EnumQuoter            (deriveEnum, EnumSafe(..))


------------------
-- Basic Types. --
------------------

data RomanDigit = I | V | X | L | C | D | M
  deriving (Eq, Show, Bounded, Ord)


-- Use Template Haskell to derive an exhaustive mapping between
-- constructors and numeric values. Enum is probably a bit of a
-- misnomer, since this class doesn’t really enumerate a type.
deriveEnum ''RomanDigit [1, 5, 10, 50, 100, 500, 1000]


---------------------------
-- Conversion functions. --
---------------------------

-- | Convert a number into the Roman numeric representation.
numerals ∷ Int → String
numerals x = toRomanPositional =≪ positionalValues x
  where
    -- Positions in Roman numerals may be represented additively or
    -- subtractively. Subtractive representation must be used if
    -- possible, additive representation is then used as a fallback.
    toRomanPositional n =  fromMaybe (additiveForm n) (subtractiveForm n)


-- The subtractive form allows prefixing only one subtractive digit,
-- and that digit must be either the same digit as the following one,
-- or it must be either of the next two smaller digits.
subtractiveForm ∷ Int → Maybe String
subtractiveForm 0 = Nothing
subtractiveForm (validatePositional → x) = do
      let (largerValue, remainValue) = toEnumGE x
      largerDigit ← toEnumMay largerValue               ∷ Maybe RomanDigit
      guard $ remainValue ∈ pred2 largerValue
      remainDigit ← toEnumMay remainValue
      return $ show =≪ [remainDigit, largerDigit]


additiveForm ∷ Int → String
additiveForm 0 = ""
additiveForm (validatePositional → x) = show smallerDigit ⧺ remainDigits
  where
    (smallerValue, remainValue) = toEnumLE x
    smallerDigit = toEnum smallerValue                        ∷ RomanDigit
    remainDigits = additiveForm remainValue


--------------
-- Utilies. --
--------------

-- runKleisli (((Kleisli $ const (Just 1)) <+> (Kleisli $ const Nothing))) ()

-- | Return a roman digit that has greater or equal value than the
-- supplied integer. Also return the remainder, if greater.
toEnumGE ∷ Int → (Int, Int)
toEnumGE x = (converted, remainder)
  where
    converted = headDef maxRoman $ dropWhile (<x) romanValues
    remainder = abs (x - fromEnum converted)
    maxRoman  = fromEnum (maxBound ∷ RomanDigit)
    romanValues = map fromEnum ([minBound .. maxBound] ∷ [RomanDigit])

-- | Return a roman digit that has lesser or equal value than the
-- supplied integer. Also return the remainder, if lesser.
toEnumLE ∷ Int → (Int, Int)
toEnumLE x = (converted, remainder)
  where
    converted = lastDef minRoman $ takeWhile (≤x) romanValues
    remainder = abs (x - fromEnum converted)
    minRoman  = fromEnum (minBound ∷ RomanDigit)
    romanValues = map fromEnum ([minBound .. maxBound] ∷ [RomanDigit])


-- | Return up to the two next predecessors to some number.
pred2 ∷ Int → [Int]
pred2 x = fromEnum <$> catMaybes [pred₁, pred₂]
    where
      xʹ    = toEnumMay x                                    ∷ Maybe RomanDigit
      pred₁ = predMay =≪ xʹ
      pred₂ = predMay =≪ pred₁


-- | Validate whether some number can be a positional value.
validatePositional ∷ Int → Int
validatePositional x
  | isPositional x = x
  | otherwise      = error "validatePositional: invalid positional value"
  where isPositional z = z `rem` 10 ↑ pred (numDigits z) ≡ 0


-- | Return the number of digits in a number, if it were represented
-- as a decimal number.
numDigits ∷ Int → Int
numDigits x
  | x <  0    = error "numDigits: undefined"
  | x < 10    = 1
  | otherwise = succ (numDigits $ x `div` 10)


-- | Return the positional value of each digit in the integer. E. g.,
-- 123 ↦ [100, 20, 3].
positionalValues ∷ Int → [Int]
positionalValues x = zipWith (×) digits multipliers
  where
    digits = map toNum (show x)
    multipliers  = reverse $ take (length digits) multipliersʹ
    multipliersʹ = 1 : map (×10) multipliersʹ

    toNum c
      | result ≥ 0 ∧ result ≤ 9 = result
      | otherwise               = error "toNum: undefined"
      where result = ord c - 48
