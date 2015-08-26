{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module Queens (boardString, canAttack) where

import           Control.Applicative
import           Control.Applicative.Unicode
import           Control.Lens
import           Control.Monad.Unicode
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Reducer
import           Data.Tuple
import           GHC.Exts
import           Prelude.Unicode
import           Prelude.Unicode.SR


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

boardStringʹ whiteQueen blackQueen = show board

    where
      board  = foldReduce [setBlack, setWhite] emptyBoard          ∷ Board Piece

      setBlack = setMay blackQueen "B"                 ∷ EndoMaybe (Board Piece)
      setWhite = setMay whiteQueen "W"

      setMay pos piece boardʹ = fmap (\p → boardʹ & ix p .~ piece) pos

      -- Is there any abstraction which allows chaining the «setMay»
      -- calls in a way that doesn’t introduce instance ambiguities
      -- like the «reducers» package does? One can define a «Maybe
      -- Position → Lens' (Board Piece) (Maybe Piece)», which can be
      -- chained, but I’m not sure it satisfies the required laws. It
      -- is also possible to define a custom composition operator for
      -- newtyped «Board Piece → Maybe (Board Piece)», but this seems
      -- overkill without making it more general.
      --
      -- Obviously, one could also have applied the lens to the empty
      -- board twice, folding the two resulting boards using the
      -- Monoid instance. But this creates two separate structures,
      -- whereas this isn’t necessary when chaining to operations
      -- instead. Unless I misunderstood the purpose of «reducers»,
      -- this is exactly what they’re there for. However, the instance
      -- ambiguities make this abstraction somewhat less satisfying
      -- than it could be.


canAttack, canAttackʹ ∷ Position
                      → Position
                      → Bool

canAttack (swap → pos₁) (swap → pos₂) =

    canAttackʹ pos₁ pos₂

canAttackʹ pos₁ pos₂ =

    -- Columns and rows could be directly checked within guards, but
    -- for the sake of compositionality this optimization is not
    -- pursued here.
    --
    -- Since the domain presupposes both pieces are queens, so that
    -- the canAttack relation is reflexive, it suffices to check in
    -- one direction.

    any (pos₁ ∈) [row pos₂, column pos₂, diagonals pos₂]


emptyBoard ∷ Board Piece
emptyBoard = Board (replicate 8 rowʹ)
    where rowʹ = replicate 8 "_"


------------
-- Types. --
------------

type Measure     = Int                                      --------------------
type Position    = (Measure, Measure)                       -- Generic synonyms.

type EndoOf    α = α → α
type EndoMaybe α = α → Maybe α

                       -- Is there an existing abstraction/name for
                       -- EndoMaybe?


newtype Piece = Piece String                                    ----------------
    deriving (Eq, IsString)                                     -- Board pieces.

instance Show Piece where
    show (Piece x) = x

instance Monoid Piece where
    mempty = Piece ""
    Piece "_" `mappend` Piece y   = Piece y
    Piece x   `mappend` Piece "_" = Piece x
    Piece x   `mappend` Piece y   = Piece (x ⊕ y)


data Board α = Board [[α]]                                         -------------
    deriving (Eq, Functor)                                         -- The board.

                       -- A Seq would be more appropriate for a chess
                       -- it would need a ZipList-like Seq wrapper.
                       -- board. This will require defining the
                       -- Applicative instance differently, though, or

instance Applicative Board where
    pure x = Board $ repeat (repeat x)
    Board rows₁ <*> Board rows₂ = Board (fmap getZipList zippedRows)
        where
          zippedRows = zipWith (⊛) (fmap ZipList rows₁) (fmap ZipList rows₂)

instance Semigroup (Board Piece) where
    board₁ <> board₂ = liftA2 (⊕) board₁ board₂

instance Monoid (Board Piece) where
    mempty  = Board (replicate 8 (replicate 8 "_"))
    mappend = (<>)

instance Reducer (EndoMaybe (Board Piece)) (EndoOf (Board Piece))
    where unit f = fromMaybe mempty ∘ f

type instance IxValue (Board α) = α
type instance Index   (Board α) = Position
instance Ixed (Board α) where
    ix (i,j) f (Board rows) =
        Board ⦷ (ix j ∘ ix i) f rows

instance Show (Board Piece) where
    show (Board rows) = showRow =≪ rows
        where showRow = (⧺"\n") ∘ intersperse ' ' ∘ (show =≪)


----------------
-- Utilities. --
----------------

column, row, diagonals ∷ Position → [Position]

column (x,_) = (x,) ⦷ [0..7]
row    (_,y) = (,y) ⦷ [0..7]

diagonals posʹ = filter (allOf both inBounds) ∘ getZipSlide $
                   traverseOf both slideUpDown posʹ

    -- This overgenerates some coordinates for the sake of
    -- conciseness. It would be nice if the inbounds filter
    -- could be put inside the traversal somehow.

    where
      slideUpDown ordinate = fmap (ordinate+) (ZipSlide [-8..8])
      inBounds = (≥0) ∧∧ (≤7)


-- | An applicative context like a ZipList, but the zipping effect
-- is run both forwards and backwards.
data ZipSlide α = ZipSlide { getZipSlide ∷ [α] }
    deriving (Show, Functor)

instance Applicative ZipSlide where

    -- Not sure if this satisfies all the relevant laws.

    pure x = ZipSlide (pure x)

    ZipSlide f <*> ZipSlide l = ZipSlide (up ⧺ down)
        where up   = getZipList $ ZipList f ⊛ ZipList l
              down = getZipList $ ZipList f ⊛ ZipList (reverse l)
