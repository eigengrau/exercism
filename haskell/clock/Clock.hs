{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}

module Clock (fromHourMin, toString) where

import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Text.Printf


data Clock = Clock { hours, minutes ∷ Int }


instance Show Clock where
    show Clock{..} = printf "%02d:%02d" hours minutes

instance Eq Clock where
    c₁ == c₂ = clockMinutes c₁ ≡ clockMinutes c₂

instance Num Clock where
    c₁ + c₂ = fromInteger (clockMinutes c₁ + clockMinutes c₂)
    c₁ - c₂ = fromInteger (clockMinutes c₁ - clockMinutes c₂)
    c₁ * c₂ = fromInteger (clockMinutes c₁ × clockMinutes c₂)

    signum = fromInteger ∘ signum    ∘ clockMinutes
    negate = fromInteger ∘ (24×60 -) ∘ clockMinutes
    abs    = fromInteger ∘ abs       ∘ clockMinutes

    fromInteger (fromIntegral → total) = Clock (h `mod` 24) m
        where
          h = total `div` 60
          m = total - 60 × h


-- | The number of minutes which have passed since midnight.
clockMinutes ∷ Integral α ⇒ Clock → α
clockMinutes Clock{..} =
    (fromIntegral hours `mod` 24) × 60 + fromIntegral minutes


fromHourMin ∷ Integral α ⇒ α → α → Clock
fromHourMin hours minutes =
    Clock (fromIntegral hours) (fromIntegral minutes)


toString ∷ Clock → String
toString = show
