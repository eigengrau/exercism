{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}

module DNA (count, nucleotideCounts) where

import qualified Data.List       as List
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Prelude.Unicode


count ∷ Char → String → Int
count (validatedNucleotide → c) (validatedString → s)=
  count' c s

    where
      count' e = length ∘ filter (≡e)


nucleotideCounts ∷ String → Map Char Int
nucleotideCounts (validatedString → s) =
  foldr addCount initialCounts s

    where
      initialCounts ∷ Map Char Int
      initialCounts = Map.fromList $ zip nucleotides (repeat 0)

      addCount ∷ Char → Map Char Int → Map Char Int
      addCount c m = Map.insert c (incrementFor c m) m

      incrementFor ∷ Char → Map Char Int → Int
      incrementFor c m = maybe 0 succ (Map.lookup c m)


nucleotides ∷ String
nucleotides = "ATCG"


validatedNucleotide ∷ Char → Char
validatedNucleotide c
  | not (isValidNucleotide c) = errorFor c
  | otherwise                 = c

  where
    isValidNucleotide = (`List.elem` nucleotides)
    errorFor c'       = error $ "invalid nucleotide " ⧺ show c'


validatedString ∷ String → String
validatedString = map validatedNucleotide
