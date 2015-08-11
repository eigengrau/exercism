{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module Phone (areaCode, number, prettyPrint) where

import Prelude.Unicode
import Data.Char


type PhoneNumber = String

areaCode ∷ PhoneNumber → String
areaCode (number → num) = take 3 number


cleanValid ∷ PhoneNumber → PhoneNumber
cleanValid = validated ∘ cleaned

number ∷ PhoneNumber → PhoneNumber
number = cleanValid


prettyPrint ∷ PhoneNumber → PhoneNumber
prettyPrint number = "(" ⧺ (show $ areaCode number) ⧺ ") " ⧺ show prefix ⧺ "-" ⧺ show remain
  where
    areaCode 

cleaned ∷ PhoneNumber → PhoneNumber
cleaned = filter isNumber


validated ∷ PhoneNumber → PhoneNumber
validated number
  | length number < 10                     = badNumber
  | length number ≡ 10                     = number
  | length number ≡ 11 ∧ head number ≡ '1' = tail number
--  | length number ≡ 11 ∧ head number ≢ '1' = badNumber
  | otherwise                              = badNumber
  where
    badNumber = "0000000000"
