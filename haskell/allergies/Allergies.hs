{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax      #-}

module Allergies (Allergen(..), isAllergicTo, allergies) where

import           Data.Bits
import           Data.Data
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe
import           Prelude.Unicode
import           Prelude.Unicode.SR


------------
-- Types. --
------------

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Ord, Show, Data)


-- I am still unsure how to define instances of Enums like this one in
-- a way that is safe yet is not bloated by a considerable amount of
-- boiler-plate (in terms of a large amount of explicit pattern
-- matches).
--
-- One possibility is to let the compiler derive an Enum instance for
-- a type «Allergenʹ», and then define a newtype, «Allergen», whose
-- Enum instance is a function of the wrapped Enum. However, this will
-- require downstream code to be changed (in this case, it would break
-- the unit tests).
--
-- Alternatively, a mapping table can be constructed programmatically,
-- capturing any underlying regularities, and this table is then used
-- in defining the Enum instance. The drawback of this is that it
-- loses some safety, since the compiler cannot check the totality of
-- the lookup-table, and new constructors will have to be added in two
-- places. A compromise would be deriving a Data instance on the type,
-- which allows enumerating the constructors programmatically. For
-- irregular mappings, defining one direction of the mapping as a case
-- expression enables the compiler to check that the mapping is total
-- (however, this reintroduces some amount of boilerplate). Another
-- disadvantage is that a naïve mapping based on Data.Map or lists is
-- probably not super efficient (maybe memoization would be appropiate
-- to fix this; still it seems the table would always be constructed
-- at runtime).
--
-- I do not have expertise in generic programming and Haskell’s
-- Generics library, so I do not know how these would fit into the
-- picture. It seems to me like the simplest way of scrapping the
-- boilerplate, remaining type-safe, and not using black voodoo would
-- involve Template Haskell; however, I could not locate an existing
-- library for this so far.
--
-- Until I achieve illumination, I shall have to stick with the
-- mapping-based approach. :<

instance Enum Allergen where

    fromEnum x = fromJust $ Map.lookup x fromAllergen
    toEnum   x = fromJust $ Map.lookup x fromBitmask

    succ x = head $ dropWhile (≤x) allergens
    pred x = last $ takeWhile (<x) allergens


-- Used internally by the Enum instance.
allergens    ∷ [Allergen]
allergens    = map fromConstr ∘ dataTypeConstrs $ dataTypeOf Eggs
fromAllergen ∷ Num α ⇒ Map Allergen α
fromAllergen = Map.fromList $ zip allergens powers2
fromBitmask  ∷ (Ord α, Num α) ⇒ Map α Allergen
fromBitmask  = Map.fromList $ zip powers2 allergens


----------
-- API. --
----------

-- Unfortunately, the test cases won’t compile due to ambiguity when a
-- polymorphic type is used, since literals in the test-suites neither
-- provide signatures nor are subject to defaulting.

--isAllergicTo ∷ (Bits α, Num α) ⇒ Allergen → α → Bool
isAllergicTo ∷ Allergen → Word → Bool
isAllergicTo allergen coded =
    (fromIntegral ∘ fromEnum $ allergen) .&. coded ≢ 0


--allergies ∷ (Num α, Bits α) ⇒ α → [Allergen]
allergies ∷ Word → [Allergen]
allergies coded = filter (`isAllergicTo` coded) allergens


----------------
-- Utilities. --
----------------

powers2 ∷ Num α ⇒ [α]
powers2 = 1 : map (×2) powers2
