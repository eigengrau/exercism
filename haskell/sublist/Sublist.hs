{-# LANGUAGE UnicodeSyntax #-}

module Sublist (Sublist(..), sublist) where


import           Control.Monad
import qualified Data.List         as List
import           Data.List.Unicode
import           Prelude.Unicode


data Sublist = Equal
             | Superlist
             | Sublist
             | Unequal
             deriving (Eq, Show, Ord)



sublist ∷ Eq α ⇒ [α] → [α] → Sublist

-- Instead of using something based on inits & tails, this solves the
-- problem using a solution to the more general problem of aligning
-- arbitrary lists.

-- Since we are attacking this based on alignments, these are special
-- cases.
sublist [] [] = Equal
sublist [] _  = Sublist
sublist _  [] = Superlist

-- If we have non-empty lists, we can solve this based on aligning
-- them.
sublist l₁ l₂
    | null alignmentsʹ = Unequal
    | otherwise        = head alignmentsʹ

  where
    -- The sublists problem expects to see the strongest possible
    -- alignment, so use the Ord instance on Sublist.
    alignmentsʹ = List.sort $ do
      -- Optimization for big lists: We are only looking for
      -- alignments rooted at zero for one of the lists. Since
      -- alignment indices are monotonically increasing, we can stop
      -- looking for alignments after the 0-based offsets are through.
      -- Using a guard instead would run into trouble with long lists,
      -- since we are sorting the results, which forces the offsets
      -- list.
      offsets ← takeWhile (someOffset (≡0)) (alignmentOffsets l₁ l₂)
      let aligned = align l₁ l₂ offsets
      guard (length aligned > 1)
      case (aligned ≡ l₁, aligned ≡ l₂) of
        (True, True)  → return Equal
        (True, False) → return Sublist
        (False, True) → return Superlist
        _             → return Unequal

    someOffset f (x,y) = f x ∨ f y


type AlignmentOffset = (Int, Int)


-- | Get pairs of offsets into l₁ and l₂ where the two lists can be
-- aligned.
alignmentOffsets ∷ Eq α ⇒ [α] → [α] → [AlignmentOffset]
alignmentOffsets l₁ l₂ = concatMap offsetsFor (l₁ ∩ l₂)
  where
    offsetsFor a = [(i₁,i₂) | i₁ ← List.findIndices (≡a) l₁
                            , i₂ ← List.findIndices (≡a) l₂]


-- | Given offsets into l₁ and l₂, produce the longest possible
-- sequence of items common to l₁ and l₂ starting at the respective
-- indices.
align ∷ Eq α ⇒ [α] → [α] → AlignmentOffset → [α]
align l₁ l₂ (n, m) = map fst ∘ takeWhile (uncurry (≡)) $ zip l₁ʹ l₂ʹ
  where
    l₁ʹ = List.drop n l₁
    l₂ʹ = List.drop m l₂
