{-# LANGUAGE UnicodeSyntax #-}

module Roman (numerals) where


import           Control.Monad
import           Control.Monad.Unicode
import           Data.Char
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Safe


------------------
-- Basic Types. --
------------------

data RomanDigit = I | V | X | L | C | D | M
                deriving (Eq, Ord, Show, Bounded)

-- The RomanDigit Enum instance is, of course, partial. This is in
-- line with other base instances for Bounded types. For safety,
-- safe{succ,pred,toEnum} are provided.
instance Enum RomanDigit where

  succ r = head $ dropWhile (≤r) romanDigits
  pred r = last $ takeWhile (≢r) romanDigits

  toEnum   v = fromJust $ Map.lookup v toRomanValue
  fromEnum r = fromJust $ Map.lookup r fromRomanValue

romanValues ∷ [Int]
romanValues = [1, 5, 10, 50, 100, 500, 1000]

romanDigits ∷ [RomanDigit]
romanDigits = [I, V, X, L, C, D, M]

toRomanValue   ∷ Map Int RomanDigit
toRomanValue   = Map.fromList $ zip romanValues romanDigits
fromRomanValue ∷ Map RomanDigit Int
fromRomanValue = Map.fromList $ zip romanDigits romanValues

maxRoman, minRoman ∷ Int
maxRoman = fromEnum (maxBound ∷ RomanDigit)
minRoman = fromEnum (minBound ∷ RomanDigit)


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
subtractiveForm x = do

  guard $ isPositional x  -- Sanity check.
  let (largerValue, remainValue) = toEnumGE x
  largerDigit ← safeToEnum largerValue                   ∷ Maybe RomanDigit
  guard $ remainValue ∈ 1 : pred2 largerValue
  remainDigit ← safeToEnum remainValue                   ∷ Maybe RomanDigit
  return $ show =≪ [remainDigit, largerDigit]


additiveForm ∷ Int → String
additiveForm 0 = ""
additiveForm x = show smallerDigit ⧺ remainDigits
  where
    (smallerValue, remainValue) = toEnumLE x
    smallerDigit = toEnum smallerValue                   ∷ RomanDigit
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


-- | Return a roman digit that has lesser or equal value than the
-- supplied integer. Also return the remainder, if lesser.
toEnumLE ∷ Int → (Int, Int)
toEnumLE x = (converted, remainder)
  where
    converted = lastDef minRoman $ takeWhile (≤x) romanValues
    remainder = abs (x - fromEnum converted)


-- | Return the two successors to some number.
pred2 ∷ Int → [Int]
pred2 x = do
      guard (x > 5)
      value ← concat (maybeToList $ sequence [pred', pred''])    ∷ [RomanDigit]
      return $ fromEnum value
    where
      x'     = safeToEnum x
      pred'  = fmap pred x'
      pred'' = fmap pred pred'


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
isPositional x = x `rem` 10 ↑ pred (numDigits x) ≡ 0


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
    digits  = map toNum (show x)
    multipliers  = reverse $ take (length digits) multipliers'
    multipliers' = 1 : map (×10) multipliers'

    toNum c
      | result ≥ 0 ∧ result ≤ 9 = result
      | otherwise               = undefined
      where result = ord c - 48
