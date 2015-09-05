{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UnicodeSyntax     #-}


module Minesweeper (annotate) where

import           Prelude.Unicode
import Data.List
import Control.Comonad
import Control.Comonad.Trans.Class
import Prelude.Unicode.SR


type Board α = Row (Row α)
type Piece = Char

data Mine = Mine deriving Show

testBoard ∷ [String]
testBoard = [ " 2*2 ",
              "25*52",
              "*****",
              "25*52",
              " 2*2 " ]

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

readBoard ∷ [String] → Board (Either Mine Int)
readBoard b = Row  [] (head rows) (tail rows)
    where
      rows = fmap readRow b
      readRow p     = Row [] (head pieces) (tail pieces)
          where pieces = (fmap readPiece) p
      readPiece '*' = Left Mine
      readPiece _   = Right 0

-- scanBoard ∷ Board (Either Mine Int) → Board (Either Mine Int)
-- scanBoard (Board rows) = Board pass₂
--     where
--       pass₁ = fmap scanRow rows
--       pass₂ = (transpose ∘ fmap scanRow ∘ transpose) pass₁
--       scanRow (Left Mine : Right b : cs) = Left Mine : Right (b+1) : scanRow cs
--       scanRow (Right a : bs@(Left Mine : _)) = Right (a+1) : scanRow bs
--       scanRow (a : bs) = a : scanRow bs
--       scanRow _ = []

printBoard ∷ Board (Either Mine Int) → [String]
printBoard = fmap printRow ∘ flatten
    where
      printRow = fmap printPiece
      printPiece (Right x)   = head (show x)
      printPiece (Left Mine) = '*'

annotate ∷ [String] → [String]
annotate = printBoard ∘ runCounts ∘ readBoard


-- http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html

data Row α = Row [α] α [α]   -- [Left] Here [Right]
    deriving Functor

                  -- Note that notationally, the list of leftward
                  -- items appears reversed. The left-neighbouring
                  -- item is the head of the list, and distance from
                  -- the center increases as the list proceeds.

instance Show α ⇒ Show (Row α) where
    show (Row ls x rs) = "Row "                ⧺
                            show (reverse ls)    ⧺
                            " ⟨" ⧺ show x ⧺ "⟩ " ⧺
                            show rs


instance Comonad Row where
   duplicate g@(Row l _ r) = Row
                                (tail ∘ take (length l + 1) $ iterate left g)
                                g
                                (tail ∘ take (length r + 1) $ iterate right g)
   extract (Row _ x _) = x


right, left ∷ Row α → Row α
right (Row as b (c:cs)) = Row (b:as) c cs
right g                 = g
left  (Row (a:as) b cs) = Row as a (b:cs)
left  g                 = g

testRow ∷ Row (Either Mine Int)
testRow = Row [Left Mine, Right 0] (Left Mine) [Right 0, Right 0]

count ∷ Row (Either Mine Int) → Either Mine Int
count (Row (Left Mine:_) (Right x) (Left Mine:_)) = Right 2
count (Row l             (Right x) (Left Mine:_)) = Right 1
count (Row (Left Mine:_) (Right x) r)             = Right 1
count (Row l             (Right x) r)             = Right x
count (Row l x r)                                 = x

testGrid ∷ Row (Row (Either Mine Int))
testGrid = Row
             [Row [] (Left Mine) [Right 0]]
             (Row [] (Right 0) [Right 0])
             [Row [] (Left Mine) [Right 0]]

--transposeGrid ∷ Row (Row α) → Row (Row α)
transposeGrid g@(Row l x r) = newGrid
    where
      flatten (Row l x r) = l ⧺ x : r
      flatGrid = (flatten ∘ fmap flatten) g
      flatGridʹ = transpose flatGrid
      newGrid = (mkRow ∘ fmap mkRow) flatGridʹ
      mkRow (x:xs) = Row [] x xs


flatten = flattenʹ ∘ fmap flattenʹ
    where flattenʹ (Row l x r) = l ⧺ x : r

runCounts = transposeGrid ∘ fmap (=>> count) ∘ transposeGrid ∘ fmap (=>> count)
  -- TODO Diagonals

-- I need a 3 x 3 neighbour structure? Theoretically this could also
-- be broken down using the existing structure, by incrementally
-- shifting rows, counting, and shifting back.
