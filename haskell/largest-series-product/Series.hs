{-# LANGUAGE UnicodeSyntax #-}

module Series (digits, slices, largestProduct) where

import           Data.Char
import           Data.Maybe
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Safe
import           Safe.Exact


digits ∷ Integral α ⇒ String → [α]
digits = fromJustNote note ∘ digitsMay
    where note = "digits: invalid input."


digitsMay ∷ Integral α ⇒ String → Maybe [α]
digitsMay = mapM digitMay


digitMay ∷ Integral α ⇒ Char → Maybe α
digitMay c
    | c ∈ ['0'..'9'] = Just ∘ fromIntegral $ ord c - ord '0'
    | otherwise      = Nothing


slices ∷ Integral α ⇒ Int → String → [[α]]
slices n = slicesʹ n ∘ digits

    where
      slicesʹ 0 _  = []
      slicesʹ n l  =
          let here = takeExactMay n l
              next = drop 1 l
          in maybe [] (: slicesʹ n next) here


largestProduct ∷ Integral α ⇒ Int → String → α
largestProduct n s  = maximumDef 1 products
    where products = fmap product (slices n s)
