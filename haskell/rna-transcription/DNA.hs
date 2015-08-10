{-# LANGUAGE UnicodeSyntax #-}

module DNA (toRNA) where


toRNA ∷ String → String
toRNA = fmap toRNA'
  where
    toRNA' 'G' = 'C'
    toRNA' 'C' = 'G'
    toRNA' 'A' = 'U'
    toRNA' 'T' = 'A'
    toRNA' _   = error "Invalid input to toRNA"
