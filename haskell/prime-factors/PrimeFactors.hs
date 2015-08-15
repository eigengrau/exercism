{-# LANGUAGE UnicodeSyntax #-}

module PrimeFactors (primeFactors) where


primeFactors ∷ Integral α ⇒ α → [α]
primeFactors x = reverse $ primeFactorsʹ x [] 2
    where
      primeFactorsʹ 1 l _ = l
      primeFactorsʹ a l d =
          case a `divMod` d of
            (result, 0) → primeFactorsʹ result (d:l) d
            _           → primeFactorsʹ a      l     (succ d)
