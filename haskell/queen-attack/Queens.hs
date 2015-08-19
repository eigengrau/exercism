{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Queens (boardString, canAttack) where

import           Control.Applicative
import           Control.Applicative.Unicode
import           Control.Lens                hiding ((&))
import           Control.Monad
import           Control.Monad.Unicode
import qualified Data.Foldable               as Fold
import qualified Data.List                   as List
import           Data.Maybe
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as Vector
import           Data.Vector.Unicode.SR
import           Debug.Trace
import           Prelude.SR
import           Prelude.Unicode
import           Prelude.Unicode.SR


------------
-- Types. --
------------

type Piece    = Char
type Position = (Int, Int)

data Board α = Board [[α]]

type instance IxValue (Board α) = α
type instance Index   (Board α) = Position

instance Eq α ⇒ Eq (Board α) where

    Board rows == Board rowsʹ =
        rows ≡ rowsʹ

instance Functor Board where

    fmap f (Board rows) =
        Board $ fmap (fmap f) rows

instance Applicative Board where

    pure x = Board (replicate 8 rowsʹ)
        where rowsʹ = replicate 8 x
        -- Alternative: Board [[x]]

    Board rows₁ <*> Board rows₂ = Board $ fmap getZipList zippedRows
        where zippedRows = zipWith (⊛) (fmap ZipList rows₁) (fmap ZipList rows₂)

instance Monoid α ⇒ Monoid (Board α) where
    mempty = Board [[]]
    board₁ `mappend` board₂ = (⊕) ⦷ board₁ ⊛ board₂

instance Foldable Board where
    foldMap f (Board rows) = foldMap (foldMap f) rows

instance Traversable Board where
    traverse fun (Board rows) = Board ⦷ traverse (traverse fun) rows

instance Ixed (Board α) where
    ix (i,j) f (Board rows) =
        Board ⦷ (ix j ∘ ix i) f rows

-- Why does this overlap with «instance (i ~ j) ⇒ Indexable i (Indexed
-- j)» from Control.Lens? E.g., arising from «emptyBoard ^.. (indexing
-- each ∘ ifiltered (\(i ∷ Position) a -> True))». How can Position be
-- type-equal with Int? Ah, I keep forgetting GHC doesn’t take
-- constraints into account for disambiguation.
instance {-# OVERLAPS #-} Indexable Int (Indexed Position) where
    indexed (Indexed f) i = f (i `mod` 8, i `div` 8)

instance FunctorWithIndex Position Board where
    imap f (Board rows) = Board (zipWith applyRow rows [0..8])
        where
          applyRow   r i = zipWith (applyCol i) r [0..8]
          applyCol i c j = f (i,j) c

instance FoldableWithIndex Position Board where
    ifoldMap f board = let Board rowsʹ = imap f board
                       in (Fold.fold $ join rowsʹ)

instance TraversableWithIndex Position Board where
    itraverse f board = let Board rowsʹ = imap f board
                        in Board ⦷ sequenceA (fmap sequenceA rowsʹ)

-- itraverse (\(i,j) a → if i > 5 then pure a else pure 'x' ) emptyBoard

instance Each (Board α) (Board α) α α where
    each f (Board rows) =
        Board ⦷ (each ∘ each) f rows

instance {-# OVERLAPPABLE #-} Show α ⇒ Show (Board α) where
    show (Board rows) =  rows ≫= \rowʹ → (rowʹ ≫= show) ⧺ "\n"
instance {-# OVERLAPPING #-} Show (Board Char) where
    show (Board rows) = rows ≫= (⧺ "\n")
instance {-# OVERLAPPING #-} Show (Board String) where
    show (Board rows) = rows ≫= (⧺ "\n") ∘ concat


----------
-- API. --
----------

boardString ∷ Maybe Position → Maybe Position → Board Piece
boardString = undefined


canAttack = undefined


emptyBoard ∷ Board Piece
emptyBoard = Board (replicate 8 rows)
    where rows = replicate 8 '_'

testBoard ∷ Board Piece
testBoard = Board ∘ take 8 $ cycle [a,b]
    where
      a = take 8 ∘ List.intersperse 'x' $ repeat '_'
      b = take 8 ∘ List.intersperse '_' $ repeat 'x'

transpose ∷ Board α → Board α
transpose (Board rows) = Board (List.transpose rows)


row ∷ Int → Lens' (Board α) [α]
row i = lens getRow setRow
    where
      getRow (Board rows)   = rows ^. ix i
      setRow (Board rows) (normalizeRowCol → r) = Board (rows & ix i .~ r)

column ∷ Int → Lens' (Board α) [α]
column i = lens getCol setCol
    where
      getCol board = transpose board ^. row i
      setCol board (normalizeRowCol → c) =
          let boardʹ = transpose board & row i .~ c
          in transpose boardʹ

normalizeRowCol ∷ [α] → [α]
normalizeRowCol v
    | lengthʹ > 8 = take 8 v
    | lengthʹ < 8 = take 8 (cycle v)
    | otherwise   = v
    where lengthʹ = length v

-- over each (over (ix 0) (+1) ∘ over (ix 1) (+2)) [[1], [2,3]]
-- [[1], [2,3]] → [[2], [3, 5]]

-- [[1], [2,3]] ^.. (each ∘ ix 0)
-- [1, 2]
-- λ> [[1], [2,3]] ^.. (each ∘ (\x -> x ∘ traceShowId))
-- [1]
-- [2,3]


getDiag ∷ [Position] → Board α → [α]
getDiag diagonalsʹ board = board ^.. indexing each ∘ ifiltered (\i _ → i∈diagonalsʹ)

setDiag ∷ [Position] → Board α → [α] → Board α
setDiag positions board c = List.foldl' (\b p → b & ix p .~ head c) board positions

-- diagonals ∷ Position → Lens (Board α) (Board α) α [α]
--diagonals ∷ Position → Lens' (Board Piece) [Piece]
diagonals ∷ Show α ⇒ Position → Lens' (Board α) [α]
diagonals (x,y) = lens (getDiag $ diagonalsʹ (x,y)) (setDiag $ diagonalsʹ (x,y))
    where

diagonalsʹ (x,y) = positions
    where
      positions = (x,y) : zip up left ⧺ zip up right ⧺ zip down left ⧺ zip down right
      down  = [y+1, y+2 .. 8]
      up    = [y-1, y-2 .. 0]
      left  = [x-1, x-2 .. 0]
      right = [x+1, x+2 .. 8]


goer ∷ Traversal' (Board Piece) [Piece]
goer f (Board rows) = (\x → Board [x])  ⦷ f ['a']
    where
      diag = zip (repeat 0) [0..]

-- putCh emptyBoard (1,3) (Just 'a')
-- List.transpose $ List.transpose $ List.reverse $ putCh emptyBoard (1,3) (Just 'a')

--  emptyBoard & (indexing each ∘ ifiltered (\(i∷Int) x → i == 1)) .~ 'x'

-- emptyBoard ^.. (indexing each ∘ ifiltered (\(i∷Int) x → i == 1))

-- TODO use a zipper for a lens less solution?

--putChar ∷ Board → Position → Char → Board
putCh board (x,y) c = atRow y (atColumn x (const c))
                -- TODO: Lens?
    where
      atRow row fun = let (a, b : remain) = splitAt row board
                      in a ⧺ fun b : remain
      atColumn y fun col = let (a, b : remain) = splitAt y col
                           in a ⧺ fun b : remain
      at i c a
          | a > i     = Nothing
          | otherwise = case a of
                          t → Just (Just c,  succ a)
                          _ → Just (Nothing, succ a)

-- over (indexing each ∘ ifiltered (\i x → i ≡ 1)) (const 99) [5..10]
-- [5..10] ^.. (indexing each ∘ ifiltered (\i x → i == 1))

-- <interactive>:1105:16:
--     Overlapping instances for Indexable Int (Indexed Position)
--       arising from a use of ‘folded’
--     Matching instances:
--       instance (i ~ j) ⇒ Indexable i (Indexed j)
--         -- Defined in ‘Control.Lens.Internal.Indexed’
--       instance Indexable Int (Indexed Position)
--         -- Defined at Queens.hs:57:10
--     In the first argument of ‘(∘)’, namely ‘folded’
--     In the second argument of ‘(^..)’, namely
--       ‘folded ∘ ifiltered (\ (Pos i) a -> traceShow i True)’
--     In the expression:
--       emptyBoard ^.. folded ∘ ifiltered (\ (Pos i) a -> traceShow i True)
