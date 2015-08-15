{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Binary (toDecimal) where

import           Data.Either.Combinators (fromRight)
import           Prelude.Unicode
import           Prelude.Unicode.SR


toDecimal ∷ Integral α ⇒ String → α
toDecimal = fromRight 0 ∘ toDecimalSafe


toDecimalSafe ∷ ∀α . Integral α ⇒ String → Either Char α
toDecimalSafe s = do
  digits ← mapM readBit s
  let positionalValues = zipWith (×) (reverse digits) powers2
  return $ sum positionalValues

    where
      -- No reason to default powers of two to Integers when only
      -- Word results, etc., are requested.
      nat     = [0..] ∷ [α]
      powers2 = map (2↑) nat


readBit ∷ Num α ⇒ Char → Either Char α
readBit '0' = Right 0
readBit '1' = Right 1
readBit c   = Left  c
