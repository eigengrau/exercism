{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module Queens (boardString, canAttack) where

import           Control.Applicative
import           Control.Applicative.Unicode
import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Control.Monad.Unicode
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Tuple
import           GHC.Exts
import           Language.Haskell.Codo
import           Prelude.Unicode
import           Prelude.Unicode.SR
import Data.Monoid
import Control.Monad.Reader
import Control.Monad.Trans

----------
-- API. --
----------

boardString, boardStringʹ ∷ Maybe Position  -- ^ White queen.
                          → Maybe Position  -- ^ Black queen.
                          → String          -- ^ The board.

boardString (fmap swap → whiteQueen) (fmap swap → blackQueen) =

    -- The exercise gives positions as (y,x) tuples, which
    -- is silly, so we swap.

    boardStringʹ whiteQueen blackQueen

boardStringʹ whiteQueen blackQueen =

    show emptyBoard  -- (putPiece "W" whiteQueen =>> putPiece "B" blackQueen) emptyBoard

  where

putPiece ∷ Piece → Maybe Position → Board Piece → Maybe (Board Piece)
putPiece piece position board =
    fmap (\p → board & ix p .~ piece) position


data Builder α β = Builder { runBuilder ∷ α → Maybe β }
instance Monoid α ⇒ Functor (Builder α) where
    fmap f (Builder g) = Builder $ fmap f ∘ g
instance Monoid α ⇒ Applicative (Builder α) where
    pure x = Builder (const $ Just x)
    Builder f <*> Builder g = Builder (\x → (f (g x)))

canAttack, canAttackʹ ∷ Position
                      → Position
                      → Bool

canAttack (swap → pos₁) (swap → pos₂) =

    canAttackʹ pos₁ pos₂

canAttackʹ pos₁ pos₂ =

    -- Columns and rows could be directly checked within
    -- guards, but for the sake of compositionality this
    -- optimization is not pursued here.
    --
    -- Since the domain presupposes both pieces are queens,
    -- so that the canAttack relation is reflexive, it
    -- suffices to check in one direction.

    any (pos₁ ∈) [row pos₂, column pos₂, diagonals pos₂]


emptyBoard ∷ Board Piece
emptyBoard = Board (replicate 8 rowʹ)
    where rowʹ = replicate 8 "_"


------------
-- Types. --
------------

type Measure  = Int                                         -- Generic synonyms.
type Position = (Measure, Measure)


newtype Piece = Piece String                                    -- Board pieces.
    deriving (Eq, IsString)

instance Show Piece where
    show (Piece x) = x

instance Monoid Piece where
    mempty = Piece ""
    Piece "_" `mappend` Piece y   = Piece y
    Piece x   `mappend` Piece "_" = Piece x
    Piece x   `mappend` Piece y   = Piece (x ⊕ y)


data Board α = Board [[α]]                                         -- The board.

instance Eq α ⇒ Eq (Board α) where
    Board rows₁ == Board rows₂ = rows₁ ≡ rows₂

instance Functor Board where
    fmap f (Board rows) = Board $ fmap (fmap f) rows

instance Applicative Board where
    pure x = Board $ repeat (repeat x)
    Board rows₁ <*> Board rows₂ = Board (fmap getZipList zippedRows)
        where
          zippedRows = zipWith (⊛) (fmap ZipList rows₁) (fmap ZipList rows₂)

instance Monoid (Board Piece) where
    mempty = Board (replicate 8 (replicate 8 "_"))
    board₁ `mappend` board₂ = liftA2 (⊕) board₁ board₂


type instance IxValue (Board α) = α
type instance Index   (Board α) = Position
instance Ixed (Board α) where
    ix (i,j) f (Board rows) =
        Board ⦷ (ix j ∘ ix i) f rows

instance Show (Board Piece) where
    show (Board rows) = showRow =≪ rows
        where showRow = (⧺"\n") ∘ intersperse ' ' ∘ (show =≪)

type BoardMaker = Board Piece → Maybe Position → Board Piece



----------------
-- Utilities. --
----------------

column, row, diagonals ∷ Position → [Position]

column (x,_) = (x,) ⦷ [0..7]
row    (_,y) = (,y) ⦷ [0..7]

diagonals pos = filter (allOf both inBounds) ∘ getZipSlide $
                  traverseOf both slideUpDown pos

    -- This overgenerates some coordinates for the sake of
    -- conciseness. It would be nice if the inbounds filter
    -- could be put inside the traversal somehow.

    where
      slideUpDown ordinate = (ordinate+) ⦷ ZipSlide [-8..8]
      inBounds = (≥0) ∧∧ (≤7)


-- | An applicative context like a ZipList, but the zipping effect
-- is run both forwards and backwards.
data ZipSlide α = ZipSlide { getZipSlide ∷ [α] }
                  deriving Show

instance Functor ZipSlide where
    fmap f (ZipSlide x) = ZipSlide (fmap f x)

instance Applicative ZipSlide where

    -- Not sure if this satisfies all the relevant laws.

    pure x = ZipSlide (pure x)

    ZipSlide f <*> ZipSlide l =  ZipSlide (up ⧺ down)
        where up   = getZipList $ ZipList f ⊛ ZipList l
              down = getZipList $ ZipList f ⊛ ZipList (reverse l)
