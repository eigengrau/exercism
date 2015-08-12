{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module ETL (transform) where

import           Data.Char
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Prelude.Unicode


transform ∷ Integral α ⇒ Map α [String] → Map String α
transform (Map.toList ∘ fmap normalizeLetters → oldFormat) =

   Map.fromList $ do
     (score, letters) ← oldFormat
     letter ← letters
     return ([letter], score)


normalizeLetters ∷ [String] → String
normalizeLetters = map toLower ∘ concat
