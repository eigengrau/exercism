{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}


module Minesweeper (annotate) where

import           Control.Applicative.Unicode
import           Control.Comonad
import           Control.Lens
import           Control.Monad
import           Control.Monad.Unicode
import           Data.Either
import           Data.Foldable               hiding (toList)
import           Data.List.Split
import           Data.Maybe
import           Data.Sequence               (Seq)
import qualified Data.Sequence               as Seq
import           Data.Sequence.Unicode
import           Data.Tuple
import           GHC.Exts
import           Language.Haskell.Codo
import           Prelude.Unicode
import           Prelude.Unicode.SR


                            -- An alternative: just use seq. what
                            -- would have been the drawback in that?

                            -- I wonder whether any of the vanilla
                            -- comonad instances would’ve subsumed
                            -- this hand-rolled instance. Store
                            -- comonad?


------------
-- Types. --
------------

data Mine = Mine deriving Show
type MineSweeperField = Either Mine Int
type MineSweeperBoard = Board MineSweeperField


data Board α = Board {
      boardRows  ∷ Seq (Seq α),
      boardFocus ∷ (Int,Int)
    } deriving (Functor, Foldable, Traversable)


instance Ixed (Board α) where
    ix (x,y) f Board{..} = (`Board` boardFocus) ⦷ (ix x ∘ ix y) f boardRows

type instance Index (Board α)   = (Int,Int)
type instance IxValue (Board α) = α


instance {-# OVERLAPPABLE #-} Show α ⇒ Show (Board α) where

    show board = fold (rows ≫= (⊳"\n"))

        where
          rows = boardRows (fmap show board)


instance {-# OVERLAPPING #-} Show MineSweeperBoard where

    show board = fold (rows ≫= (⊳"\n"))

        where
          rows = boardRows (fmap showField board)
          showField (Left Mine) = "*"
          showField (Right c)   = show c


instance Show (Board MineSweeperBoard) where

    show = foldMap (⧺"\n") ∘ fmap show


instance Read MineSweeperBoard where

    readsPrec _ boardString = [(Board board (0,0), "")]

        where
          board = fromList $ fmap (fromList ∘ readLine) (lines boardString)

          readLine     = fmap readChar
          readChar '*' = Left Mine
          readChar _   = Right 0


instance Comonad Board where

    extract board@Board{..} =
        (boardRows `Seq.index` rowFocus board) `Seq.index` colFocus board

    duplicate board@Board{..} = Board subBoards (rowFocus board, colFocus board)

        where
          allFoci   = flip (,) ⦷ [0..pred (boardHeight board)] ⊛
                        [0..pred (boardWidth board)]

          subBoards = fromList ∘ fmap fromList ∘ chunksOf (boardWidth board) $
                        fmap (Board boardRows) allFoci


----------------------
-- Cursor movement. --
----------------------

-- | Move focus.
setFocus ∷ (Int,Int) → Board α → Board α
setFocus (col,row) board@Board{..} = Board boardRows (colʹ,rowʹ)
    where
      colʹ = col `mod` boardWidth  board
      rowʹ = row `mod` boardHeight board


-- | Move focus relative to current focus.
boardGo ∷ (Int,Int) → Board α → Board α
boardGo (col,row) board =
    setFocus (colFocus board + col, rowFocus board + row) board


-- | Predefined movements.
boardLeft, boardRight, boardUp, boardDown ∷ Board α → Board α
boardRight = boardGo ( 1, 0)
boardLeft  = boardGo (-1, 0)
boardDown  = boardGo ( 0, 1)
boardUp    = boardGo ( 0,-1)

bLeft  b
    | colFocus b ≤ 0 = Nothing
    | otherwise      = Just (extract $ boardLeft b)
bRight b
    | colFocus b ≥ pred (boardWidth b) = Nothing
    | otherwise                        = Just (extract $ boardRight b)
bUp b
    | rowFocus b ≤ 0 = Nothing
    | otherwise      = Just (extract $ boardUp b)
bDown b
    | rowFocus b ≥ pred (boardHeight b) = Nothing
    | otherwise                         = Just (extract $ boardDown b)


----------------------
-- Querying boards. --
----------------------

rowFocus, colFocus ∷ Board α → Int
rowFocus Board{..} = snd boardFocus
colFocus Board{..} = fst boardFocus


boardNull ∷ Board α → Bool
boardNull Board{..} = Seq.null boardRows


boardWidth, boardHeight ∷ Board α → Int
boardHeight = Seq.length ∘ boardRows
boardWidth  = Seq.length ∘ (`Seq.index` 0) ∘ boardRows


diags ∷ Board α → [α]
diags = [codo| board ⇒
   up ← bUp board
   (join ∘ extract → lu) ← bLeft up
   (join ∘ extract → ru) ← bRight up

   down ← bDown board
   (join ∘ extract → ld) ← bLeft down
   (join ∘ extract → rd) ← bRight down

   catMaybes [lu, ru, ld, rd]
 |]


straights ∷ Board α → [α]
straights = [codo| board ⇒
    (extract → up)    ← bUp board
    (extract → down)  ← bDown board
    (extract → left)  ← bLeft board
    (extract → right) ← bRight board

    catMaybes [up, down, left, right]
  |]


surroundings ∷ Board α → [α]
surroundings = [codo| board ⇒
    (extract → s) ← straights board
    (extract → d) ← diags board
    s ⧺ d
  |]


countMines ∷ MineSweeperBoard → Either Mine Int
countMines board = case extract board of
                     Right _    → Right (length ∘ lefts ∘ surroundings $ board)
                     Left  Mine → Left Mine


--------------------------
-- Constructing boards. --
--------------------------

readBoard ∷ [String] → MineSweeperBoard
readBoard = read ∘ ((⧺"\n") =≪)


annotate ∷ [String] → [String]
annotate (readBoard → board)
    | boardNull board = []
    | otherwise       = (lines ∘ fmap (replace '0' ' ') ∘ show)
                          (board =≫ countMines)
    where
      replace a b i
          | i ≡ a     = b
          | otherwise = i


----------------
-- Utilities. --
----------------

(=≫) ∷ Comonad ω ⇒ ω α → (ω α → β) → ω β
(=≫) = (=>>)
infixl 1 =≫


------------------
-- Test boards. --
------------------

surrounded, cross ∷ [String]

surrounded = [ "***"
             , "*8*"
             , "***"
             ]

cross = [ " 2*2 "
        , "25*52"
        , "*****"
        , "25*52"
        , " 2*2 "
        ]
