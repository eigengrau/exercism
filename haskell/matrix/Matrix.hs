{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Matrix (
    -- Inject GHC.Exts.{fromString,fromList} into the test suite.
    module GHC.Exts,
    module Matrix
  ) where


import qualified Data.List              as List
import           Data.List.Split
import           Data.Vector            (Vector, empty, head, null, (!))
import           Data.Vector.Unicode.SR
import           GHC.Exts
import           Prelude                hiding (head, null, tail)
import           Prelude.Unicode
import           Prelude.Unicode.SR


------------
-- Types. --
------------

data Matrix α = Matrix (Vector (Vector α))                  -- A vector of rows.
    deriving (Show, Functor, Eq)


instance {-# OVERLAPPABLE #-} Read α ⇒ Read (Matrix α) where
    readsPrec _ s = [(Matrix (toVector s), "")]

        where
          toVector = fromList ∘ fmap fromList ∘ fmap (fmap read) ∘
                       fmap words ∘ lines


instance {-# OVERLAPPING #-} Read (Matrix String) where
    readsPrec _ s = [(Matrix (toVector s), "")]

        where
          toVector = fromList ∘ fmap fromList ∘ fmap readStrings ∘ lines

          readStrings "" = []
          readStrings s
              | List.null result = []
              | otherwise        = here : readStrings remain
              where result@(~[(here, remain)]) = reads s


instance {-# OVERLAPPABLE #-} Read α ⇒ IsString (Matrix α) where
    fromString = read


instance {-# OVERLAPPING #-} IsString (Matrix String) where
    fromString = read


instance IsList (Matrix α) where
    type Item (Matrix α) = [α]
    fromList rows        = Matrix $ fromList (fmap fromList rows)
    toList (Matrix rows) = fmap toList (toList rows)


instance Foldable Matrix where
    foldMap f (Matrix rows) = foldMap (foldMap f) rows


instance Traversable (Matrix) where
    traverse f (Matrix rows) = Matrix ⦷ traverse (traverse f) rows


----------
-- API. --
----------

row ∷ Int → Matrix α → Vector α
row i (Matrix v) = v ! i


column ∷ Int → Matrix α → Vector α
column i (Matrix v) = fmap (! i) v


rows ∷ Matrix α → Int
rows (Matrix v) = length v


cols ∷ Matrix α → Int
cols (Matrix v)
     | null v = 0
     | otherwise = length (v ! 0)


shape ∷ Matrix α → (Int, Int)
shape = rows ↭ cols


transpose ∷ Matrix α → Matrix α
transpose (Matrix v) = Matrix (transposeVector v)


reshape ∷ (Int,Int) → Matrix β → Matrix β
reshape (nRows, nCols) =
    fromList ∘ take nRows ∘ chunksOf nCols ∘ foldMap (:[])


flatten ∷ Matrix α → Vector α
flatten = foldMap (⊳ empty)


----------------
-- Utilities. --
----------------

transposeVector ∷ Vector (Vector α) → Vector (Vector α)
transposeVector rows
    | null rows = empty
    | otherwise = fromList $ fmap getCol [0 .. pred nCols]
    where
      nCols    = length (head rows)
      getCol i = fmap (!i) rows
