{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax      #-}
{-# LANGUAGE ViewPatterns       #-}

module Allergies (Allergen(..), isAllergicTo, allergies) where

import           Data.Bits
import           Data.Data
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe
import           Prelude.Unicode
import           Prelude.Unicode.SR


data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Ord, Show, Data)

instance Enum Allergen where
    succ x = head $ dropWhile (≤x) allergens
    pred x = last $ takeWhile (<x) allergens
    fromEnum x = fromJust $ Map.lookup x fromAllergen
    toEnum   x = fromJust $ Map.lookup x fromBitmask

allergens ∷ [Allergen]
allergens = map fromConstr (dataTypeConstrs $ dataTypeOf Eggs)
bitMasks  ∷ Num α ⇒ [α]
bitMasks  = 1 : map (×2) bitMasks

fromAllergen ∷ Num α ⇒ Map Allergen α
fromAllergen = Map.fromList $ zip allergens bitMasks
fromBitmask  ∷ (Ord α, Num α) ⇒ Map α Allergen
fromBitmask  = Map.fromList $ zip bitMasks allergens


--Unfortunately, the test cases won’t compile due to ambguity if a
--polymorphic type is used.

--isAllergicTo ∷ (Bits α, Num α) ⇒ Allergen → α → Bool
isAllergicTo ∷ Allergen → Int → Bool
isAllergicTo (fromIntegral ∘ fromEnum → allergen) coded =
    allergen .&. coded ≢ 0


--allergies ∷ (Num α, Bits α) ⇒ α → [Allergen]
allergies ∷ Int → [Allergen]
allergies coded = filter (`isAllergicTo` coded) allergens
