{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

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
  deriving (Eq, Show, Bounded, Ord)


-- The RomanDigit Enum instance is, of course, partial. This is in
-- line with other base instances for Bounded types. For safety,
-- safe{succ,pred,toEnum} are provided.
instance Enum RomanDigit where

  succ r = head $ dropWhile (≤r) romanDigits
  pred r = last $ takeWhile (≢r) romanDigits

  toEnum   v = fromJust $ Map.lookup v toRomanValue
  fromEnum r = fromJust $ Map.lookup r fromRomanValue

  enumFrom x = enumFromTo x maxBound
  enumFromTo x y
    | x ≡ y     = [x]
    | x > y     = x : enumFromTo (pred x) y ∷ [RomanDigit]
    | otherwise = x : enumFromTo (succ x) y ∷ [RomanDigit]

  enumFromThenTo = error "enumFromThenTo: undefined"           -- Feeling lazy.

-- While the enum instance is provided as a nicer interface, these are
-- used internally by the Enum instance so that the pattern matches
-- don’t have to be given explicitly. It’s a bit unsafer though, since
-- «fromJust» is then used when defining «fromEnum».
toRomanValue   ∷ Map Int RomanDigit
toRomanValue   = Map.fromList $ zip romanValues romanDigits
fromRomanValue ∷ Map RomanDigit Int
fromRomanValue = Map.fromList $ zip romanDigits romanValues

-- Range notation cannot be used here, since the Enum instance uses
-- it; this would cause endless recursion.
romanDigits ∷ [RomanDigit]
romanDigits = [I, V, X, L, C, D, M]
romanValues ∷ [Int]
romanValues = [1, 5, 10, 50, 100, 500, 1000]


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
    smallerDigit = toEnum smallerValue                    ∷ RomanDigit
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

-- | Return a roman digit that has lesser or equal value than the
-- supplied integer. Also return the remainder, if lesser.
toEnumLE ∷ Int → (Int, Int)
toEnumLE x = (converted, remainder)
  where
    converted = lastDef minRoman $ takeWhile (≤x) romanValues
    remainder = abs (x - fromEnum converted)
    minRoman  = fromEnum (minBound ∷ RomanDigit)


-- | Return the two successors to some number.
-- The type Int → (Int, Int) would be more natural, and safer;
-- however, then one could not easily write «a ∈ pred2 b». Is there
-- some fancy abstraction for containment checks on tuples? Lens?
pred2 ∷ Int → [Int]
pred2 x = liftM fromEnum $ guard (x > 5) ≫ pred₁ ⧺ pred₂
    where
      xʹ    = maybeToList (safeToEnum x)                         ∷ [RomanDigit]
      pred₁ = fmap pred xʹ
      pred₂ = fmap pred pred₁


-- There is also prelude-safeenum, but that one replaces the Prelude
-- functions and is cumbersome with non-bounded Enums. Is there some
-- nice library that provides a SafeBoundedEnum class for these cases?
safeSucc ∷ (Eq α, Enum α, Bounded α) ⇒ α → Maybe α
safeSucc x | x ≡ maxBound = Nothing
           | otherwise    = Just (succ x)

safePred ∷ (Eq α, Enum α, Bounded α) ⇒ α → Maybe α
safePred x | x ≡ minBound = Nothing
           | otherwise    = Just (pred x)

safeToEnum ∷ ∀α . (Bounded α, Enum α) ⇒ Int → Maybe α
safeToEnum x
    | x ≥ minBoundʹ ∧ x ≤ maxBoundʹ = Just (toEnum x)
    | otherwise                     = Nothing
    where
      minBoundʹ = fromEnum (minBound ∷ α)
      maxBoundʹ = fromEnum (maxBound ∷ α)


-- | Validate whether some number can be a positional value.
isPositional ∷ Int → Bool
isPositional x = x `rem` 10 ↑ pred (numDigits x) ≡ 0


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
