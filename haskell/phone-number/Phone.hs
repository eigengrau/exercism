{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module Phone (areaCode, number, prettyPrint) where

import           Data.Char
import           Prelude.Unicode
import           Text.Printf


type PhoneNumber = String


--------------
-- Parsing. --
--------------

areaCode ∷ PhoneNumber → String
areaCode phoneNumber = areaCodeʹ
    where
      (areaCodeʹ, _, _) = splitNumber phoneNumber


prefix ∷ PhoneNumber → String
prefix phoneNumber = prefixʹ
    where
      (_, prefixʹ, _) = splitNumber phoneNumber


subscriber ∷ PhoneNumber → String
subscriber phoneNumber = subscriberʹ
    where
      (_, _, subscriberʹ) = splitNumber phoneNumber


splitNumber ∷ PhoneNumber → (String, String, String)
splitNumber (validated ∘ cleaned → phoneNumber) =
    (areaCodeʹ, prefixʹ, subscriberʹ)

    where
      (areaCodeʹ,    remain) = splitAt 3 phoneNumber
      (prefixʹ, subscriberʹ) = splitAt 3 remain


---------------
-- Printing. --
---------------

prettyPrint ∷ PhoneNumber → PhoneNumber
prettyPrint phoneNumber =
    printf "(%s) %s-%s"
      (areaCode   phoneNumber)
      (prefix     phoneNumber)
      (subscriber phoneNumber)


-----------------
-- Validation. --
-----------------

number ∷ PhoneNumber → PhoneNumber
number = validated ∘ cleaned

cleaned ∷ PhoneNumber → PhoneNumber
cleaned = filter isNumber


validated ∷ PhoneNumber → PhoneNumber
validated phoneNumber
  | length phoneNumber < 10  = badNumber
  | length phoneNumber ≡ 10  = phoneNumber
  | length phoneNumber ≡ 11
    ∧ head phoneNumber ≡ '1' = tail phoneNumber
  | otherwise                = badNumber

  where
    badNumber = "0000000000"
