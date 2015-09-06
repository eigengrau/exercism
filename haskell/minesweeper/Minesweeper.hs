{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}


module Minesweeper (annotate) where

import           Control.Applicative.Unicode
import           Control.Comonad
import           Control.Monad.Unicode
import           Data.Either
import           Data.Foldable               hiding (toList)
import           Data.List.Split
import           Data.Sequence               (Seq)
import qualified Data.Sequence               as Seq
import           Data.Sequence.Unicode
import           GHC.Exts
import           Prelude.Unicode
import           Prelude.Unicode.SR


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
          allFoci   = (,) ⦷ [0..pred (boardWidth board)] ⊛
                        [0..pred (boardHeight board)]
          subBoards = fromList ∘ fmap fromList ∘ chunksOf (boardWidth board) $
                        fmap (Board boardRows) allFoci


----------------------
-- Cursor movement. --
----------------------

-- | Move focus absolutely.
setFocus ∷ (Int,Int) → Board α → Board α
setFocus (col,row) Board{..} = Board boardRows (col,row)


-- | Move focus relatively.
boardGo ∷ (Int,Int) → Board α → Board α
boardGo (col,row) board =
    setFocus (colFocus board + col, rowFocus board + row) board


-- | Predefined movements.
boardLeft, boardRight, boardUp, boardDown ∷ Board α → Board α
boardRight = boardGo ( 1, 0)
boardLeft  = boardGo (-1, 0)
boardDown  = boardGo ( 0, 1)
boardUp    = boardGo ( 0,-1)


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


surroundings ∷ Board α → [α]
surroundings board@Board{..} = foldMap ((:[]) ∘ extract) surroundingsʹ

    where
      surroundingsʹ = fmap (`boardGo` board) steps
      dirs  = [-1,0,1]
      steps = filter inBounds ((,) ⦷ dirs ⊛ dirs)
      inBounds ((colFocus board +) → x, (rowFocus board +) → y) =
          x ≥ 0 ∧ x < boardWidth  board ∧
          y ≥ 0 ∧ y < boardHeight board


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
