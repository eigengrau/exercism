{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module Garden (
    garden,
    defaultGarden,
    lookupPlants,
    Plant(..)
  ) where

import qualified Data.List       as List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe
import           Prelude.Unicode


------------
-- Types. --
------------

type Child = String

-- This enforces 4 cups per child as an invariant.
type Garden = Map Child (Cup, Cup, Cup, Cup)
type GardenSpec = String

data Cup = Cup { getPlant ∷ Plant }
           deriving (Show, Eq)

data Plant = Violets
           | Radishes
           | Clover
           | Grass
           deriving (Show, Eq)

instance Read Plant where
    readsPrec _ "V" = pure (Violets,  "")
    readsPrec _ "R" = pure (Radishes, "")
    readsPrec _ "C" = pure (Clover,   "")
    readsPrec _ "G" = pure (Grass,    "")
    readsPrec _ _   = error "readsPrec: invalid Plant specification."


----------
-- API. --
----------

garden ∷ [Child] → String → Garden
garden (List.sort → children) (lines → sillSpec)
    | length sillSpec ≢ 2 = error "garden: invalid sill specification."
    | otherwise           = Map.fromList $ zip children childwiseCups
    where
      childwiseCups = zipWith concatʹ (pairwise row₁) (pairwise row₂)
      pairwise      = map toTuple ∘ chunksOf 2

      concatʹ (a,b) (c,d) = (a,b,c,d)

      toTuple l@(~[a,b])
          | length l ≡ 2 = (a,b)
          | otherwise    = error "toTuple: invalid list length."

      [line₁, line₂] = sillSpec
      row₁    = map readCup line₁
      row₂    = map readCup line₂
      readCup = Cup ∘ read ∘ pure


lookupPlants ∷ Child → Garden → [Plant]
lookupPlants = map getPlant ∘ fromMaybe [] ∘ fmap fromTuple ∘: Map.lookup
    where fromTuple (a,b,c,d) = [a,b,c,d]


defaultGarden ∷ GardenSpec → Garden
defaultGarden = garden defaultChildren
    where defaultChildren = [
             "Alice", "Bob", "Charlie", "David", "Eve",
             "Fred", "Ginny", "Harriet", "Ileana", "Joseph",
             "Kincaid", "Larry"
            ]


----------------
-- Utilities. --
----------------

-- | Skewed-arity function composition.
-- Sadly ₁∘₂ is not a legal operator identifier.  ʔ ; ᴥ ; ʕ
(∘:) ∷ (γ → σ) → (α → β → γ) → α → β → σ
(f ∘: g) a b = f (g a b)
infixr 8 ∘:
