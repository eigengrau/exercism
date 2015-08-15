{-# LANGUAGE UnicodeSyntax #-}

module Raindrops (convert) where

import           Control.Monad
import           Control.Monad.Unicode
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           PrimeFactors          (primeFactors)


convert ∷ (Show α, Integral α) ⇒ α → String
convert n
    | null plingPlangPlong = show n
    | otherwise            = plingPlangPlong
    where
      plingPlangPlong =
          (guard (3 ∈ primeFactorsʹ) ≫ "Pling") ⊕
          (guard (5 ∈ primeFactorsʹ) ≫ "Plang") ⊕
          (guard (7 ∈ primeFactorsʹ) ≫ "Plong")

      primeFactorsʹ = primeFactors n
