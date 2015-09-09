{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
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
import           GHC.Exts
import           Language.Haskell.Codo
import           Prelude.Unicode
import           Prelude.Unicode.SR          hiding ((‼))


----------------
-- Rationale. --
----------------

-- While using a structure based on «Seq (Seq α)» would be sufficient to
-- conveniently address and efficiently retrieve local neighbourhoods inside a
-- larger, grid-like structure, I was eager to dabble with Comonads a bit.
-- Having heard that a Comonadic interface expresses structurally local
-- computations which depend on a global context, and having read an interesting
-- blog post where Comonads were used to model the grid-like structure and
-- bottom-up logics of cellular automata, the Minesweeper domain seemed like
-- good fit to try defining a Comonad instance for this the minesweeper board.
--
-- The implementation is based on a comonadic interface to the game board data
-- type, which is a grid represented as a «Seq (Seq α)». The game board type
-- incorporates a notion of a /cursor/, which designates a focused position on
-- the board.
--
--   • The value currently focused can be retrieved using the «extract»
--     function.
--
--   • Applying «extend» to any function «GameBoard α → β» will turn that
--     function into a function «GameBoard α → GameBoard β». The resulting
--     function will run the original computation with the cursor set to each
--     possible cell of the board, and then stitches local results back together
--     into a coherent global structure. The operator =≫ is equivalent to «flip
--     extend».
--
-- To illustrate: when «peekRight» is function which returns the cell to the
-- right of the current cursor position, «gameBoard =≫ peekRight» designates the
-- original gameBoard with each cell’s value replaced with the value to its
-- right. Analogously, the following will draw an arrow pointing toward a mine
-- to the right:
--
--     gameBoard =≫ \board →
--       if (isMine ∘ peekRight) board
--       then '→'
--       else '_'
--
-- What this interface affords is that the «stiching back together» part is
-- managed by the Comonad interface. Additionally, while an extended function
-- can access the global environment, it can affect only the local substructure
-- for which it is run.
--
-- Open questions:
--
-- • I wonder whether any of the vanilla Comonad instances would have subsumed
--   this hand-rolled instance. Store comonad? Env?


------------
-- Types. --
------------

data Mine = Mine deriving (Show, Eq)
type MineSweeperField = Either Mine Int
type MineSweeperBoard = Board MineSweeperField


data Board α = Board {
      boardRows  ∷ Seq (Seq α),
      boardFocus ∷ (Int,Int)
    } deriving (Eq, Functor, Foldable, Traversable)


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
        (boardRows ‼ rowFocus board) ‼ colFocus board

    -- | Duplication strategy: Set subordinate focus to the coordinates of the
    -- superordinate cell.
    duplicate board@Board{..} = Board subBoards (colFocus board, rowFocus board)

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
boardWidth  = Seq.length ∘ (‼0) ∘ boardRows


peekLeft, peekRight, peekUp, peekDown ∷ Board α → Maybe α
peekLeft board
    | colFocus board ≤ 0 = Nothing
    | otherwise = Just (extract $ boardGo (-1, 0) board)
peekRight board
    | colFocus board ≥ pred (boardWidth board) = Nothing
    | otherwise = Just (extract $ boardGo ( 1, 0) board)
peekUp board
    | rowFocus board ≤ 0 = Nothing
    | otherwise = Just (extract $ boardGo ( 0,-1) board)
peekDown board
    | rowFocus board ≥ pred (boardHeight board) = Nothing
    | otherwise = Just (extract $ boardGo ( 0, 1) board)


peekDiag ∷ Board α → [α]
peekDiag = [codo| board ⇒
   up ← peekUp board
   (join ∘ extract → lu) ← peekLeft up
   (join ∘ extract → ru) ← peekRight up

   -- «join» is needed since each peek introduces another Maybe level.
   -- TODO: Use ComonadTrans instead?

   down ← peekDown board
   (join ∘ extract → ld) ← peekLeft down
   (join ∘ extract → rd) ← peekRight down

   catMaybes [lu, ru, ld, rd]
 |]


peekStraight ∷ Board α → [α]
peekStraight = [codo| board ⇒
    (extract → up)    ← peekUp board
    (extract → down)  ← peekDown board
    (extract → left)  ← peekLeft board
    (extract → right) ← peekRight board

    catMaybes [up, down, left, right]
  |]


peekSurroundings ∷ Board α → [α]
peekSurroundings = [codo| board ⇒
    (extract → s) ← peekStraight board
    (extract → d) ← peekDiag board
    s ⧺ d
  |]


-- | Count the mines surrounding the focused cell.
countMines ∷ MineSweeperBoard → Either Mine Int
countMines board =
    case extract board of
      Right _    → Right ∘ length ∘ lefts ∘ peekSurroundings $ board
      Left  Mine → Left Mine


--------------------------
-- Constructing boards. --
--------------------------

readBoard ∷ [String] → MineSweeperBoard
readBoard = read ∘ ((⧺"\n") =≪)


annotate ∷ [String] → [String]
annotate (readBoard → board)
    | boardNull board = []
    | otherwise       = lines ∘ fmap (replace '0' ' ') ∘ show $
                          board =≫ countMines
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


(‼) ∷ Seq α → Int → α
(‼) = Seq.index


------------------
-- Test boards. --
------------------

surrounded, cross ∷ [String]

surrounded = [ "***"
             , "*8*"
             , "***" ]

cross = [ " 2*2 "
        , "25*52"
        , "*****"
        , "25*52"
        , " 2*2 " ]
