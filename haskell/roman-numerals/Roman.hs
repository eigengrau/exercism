{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module Roman (numerals) where


import           Control.Monad
import           Control.Monad.Unicode
import           Data.Char
import           Data.Maybe
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Safe


------------------
-- Basic Types. --
------------------

data RomanDigit = I | V | X | L | C | D | M
                deriving (Eq, Ord, Show, Bounded)

instance Enum RomanDigit where

  succ v = dropWhile (≢v) romanDigits ‼ 1
  pred v = last (takeWhile (≢v) romanDigits)

  toEnum   v = fromJust $ lookup v toRomanValue
  fromEnum r = fromJust $ lookup r fromRomanValue

romanValues ∷ [Int]
romanValues = [1, 5, 10, 50, 100, 500, 1000]

romanDigits ∷ [RomanDigit]
romanDigits = [I, V, X, L, C, D, M]

toRomanValue   ∷ [(Int, RomanDigit)]
toRomanValue   = zip romanValues romanDigits
fromRomanValue ∷ [(RomanDigit, Int)]
fromRomanValue = zip romanDigits romanValues

maxRoman, minRoman ∷ Int
maxRoman = fromEnum (maxBound ∷ RomanDigit)
minRoman = fromEnum (minBound ∷ RomanDigit)


---------------------------
-- Conversion functions. --
---------------------------

-- | Convert a number into the Roman numeric representation.
numerals ∷ Int → String
numerals x = positionalValues x ≫= toRomanPositional
  where
    -- Positions in Roman numerals may be represented additively or
    -- subtractively. Subtractive representation must be used if
    -- possible, additive representation is then used as a fallback.
    toRomanPositional n =  additiveForm n `fromMaybe` subtractiveForm n


subtractiveForm ∷ Int → Maybe String
subtractiveForm 0 = Nothing
subtractiveForm x = do

  guard (isPositional x)
  let (largerValue, remainValue) = toEnumGE x ∷ (Int, Int)
  (show → largerDigit) ← safeToEnum largerValue ∷ Maybe RomanDigit
  guard (remainValue ∈ (1 : pred2 largerValue))
  (show → remainDigit) ← safeToEnum remainValue ∷ Maybe RomanDigit
  return $ remainDigit ⧺ largerDigit



additiveForm ∷ Int → String
additiveForm 0 = ""
additiveForm x = smallerDigit ⧺ remainDigits
  where
    (smallerValue, remainValue) = toEnumLE x
    smallerDigit  = show (toEnum smallerValue ∷ RomanDigit)
    remainDigits ∷ String
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
    converted ∷ Int
    converted = headDef maxRoman $ dropWhile (<x) romanValues
    remainder ∷ Int
    remainder = abs (x - fromEnum converted)


-- | Return a roman digit that has lesser or equal value than the
-- supplied integer. Also return the remainder, if lesser.
toEnumLE ∷ Int → (Int, Int)
toEnumLE x = (converted, remainder)
  where
    converted = lastDef minRoman $ takeWhile (≤x) romanValues
    remainder = abs (x - fromEnum converted)

-- | Return the two successors to some number.
pred2 ∷ Int → [Int]
pred2 x = let x'     = safeToEnum x      ∷ Maybe RomanDigit
              pred'  = fmap pred x'     ∷ Maybe RomanDigit
              pred'' = fmap pred pred'  ∷ Maybe RomanDigit
          in do
            guard (x > 5)
            value ← concat (maybeToList $ sequence [pred', pred''])
            return $ fromEnum value


-- There is also prelude-safeenum, but this replaces the Prelude
-- functions and is cumbersome with non-bounded Enums.
safeSucc ∷ (Eq α, Enum α, Bounded α) ⇒ α → Maybe α
safeSucc x | x ≡ maxBound = Nothing
           | otherwise    = Just (succ x)

safePred ∷ (Eq α, Enum α, Bounded α) ⇒ α → Maybe α
safePred x | x ≡ minBound = Nothing
           | otherwise    = Just (pred x)

safeToEnum ∷ (Enum α, Bounded α) ⇒ Int → Maybe α
safeToEnum x | x > minBound ∧ x < maxBound = Just (toEnum x)
             | otherwise = Nothing


-- | Validate whether some number can be a positional value.
isPositional ∷ Int → Bool
isPositional x = x `rem` 10↑(numDigits x - 1) ≡ 0


-- | Return the number of digits in a number, if it were represented
-- as a decimal number.
numDigits ∷ Int → Int
numDigits x
  | x < 10    = 1
  | otherwise = succ (numDigits $ x `div` 10)


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
