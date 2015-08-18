{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Queen2 where

import Prelude.Unicode
import Prelude.Unicode.SR
import Prelude.SR
import Control.Lens hiding ((&))
import Data.Maybe
import qualified Data.List as List
import Control.Monad.Unicode


type Piece = Char
newtype Board α = Board [[α]]

instance Functor Board where
    fmap f (Board x) = Board $ fmap (fmap f) x

instance {-# OVERLAPPABLE #-} Show α ⇒ Show (Board α) where
    show (Board l) = concat $ do
      row ← l
      return $ (concatMap show row) ⧺ "\n"

instance {-# OVERLAPPING #-} Show (Board Char) where
    show (Board l) = l ≫= (⧺ "\n")

instance {-# OVERLAPPING #-}Show (Board String) where
    show (Board l) = l ≫= (⧺ "\n") ∘ concat



board ∷ Board Piece
board = Board (replicate 8 (replicate 8 '_'))

row ∷ Int → Lens' (Board Piece) [Piece]
row i = lens getRow setRow
    where
      getRow (Board l)= l‼i
      setRow (Board l) r = (Board $ l & ix i .~ r)

column ∷ Int → Lens' (Board Piece) [Piece]
column i = lens getCol setCol
    where
      getCol (Board l) = (Board $ List.transpose l) ^. row i
      setCol (Board l)  c =
          let Board lʹ = (Board $ List.transpose l) & row i .~ c
          in Board (List.transpose lʹ)


-- bla = lens (\(Board x) → Board (head x)) (\x n → n : fmap tail x)

type instance IxValue (Board α) = α
type instance Index (Board α) = (Int,Int)

instance Ixed (Board α) where
    ix (i,j) f (Board l) = Board <$> (ix i ∘ ix j) f l

instance Each (Board α) (Board α) α α where
    each f (Board l) = Board <$> (each ∘ each) f l



-- TODO use a zipper for a lens less solution?
