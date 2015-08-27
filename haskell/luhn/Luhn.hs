{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module Luhn (
    checkDigit,
    addends,
    checksum,
    isValid,
    create
  ) where

import           Control.Applicative
import           Control.Applicative.Unicode
import           Prelude.Unicode
import           Prelude.Unicode.SR


checkDigit ∷ Integral α ⇒ α → α
checkDigit = (`mod` 10)


addends ∷ Integral α ⇒ α → [α]
addends i = reverse (getZipList processed)

    where
      processed  = ZipList operations ⊛ ZipList (digitValues i)

      operations = cycle [id, duplicate]

      duplicate ((×2) → n)
          | n ≥ 10    = n - 9
          | otherwise = n

      digitValues n
          | n < 0     = undefined
          | n < 10    = [n]
          | otherwise = here : digitValues next
          where (next, here) = n `divMod` 10


checksum ∷ Integral α ⇒ α → α
checksum = checkDigit ∘ sum ∘ addends


isValid ∷ Integral α ⇒ α → Bool
isValid = (≡0) ∘ checksum


create ∷ Integral α ⇒ α → α
create n = base + remain

    where
      base  = n × 10
      remain = (10 - checksum base) `mod` 10
