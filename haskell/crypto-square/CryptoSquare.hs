{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module CryptoSquare (
    normalizePlaintext,
    squareSize,
    ciphertext,
    normalizeCiphertext,
    plaintextSegments
  ) where

import           Data.Char
import qualified Data.List          as List
import           Data.List.Split
import           Prelude.Unicode
import           Prelude.Unicode.SR


ciphertext ∷ String → String
ciphertext = concat ∘ encipher ∘ normalizePlaintext


normalizeCiphertext ∷ String → String
normalizeCiphertext = unwords ∘ encipher ∘ normalizePlaintext


normalizePlaintext ∷ String → String
normalizePlaintext = map toLower ∘ filter isAlphaNum


encipher ∷ String → [String]
encipher s = List.transpose $ chunksOf (squareSize s) s


plaintextSegments ∷ String → [String]
plaintextSegments (normalizePlaintext → s) =
    chunksOf (squareSize s) s


-- | Compute a good chunk size which will accommodate the string in a
-- square.
squareSize ∷ Integral α ⇒ String → α
squareSize (List.genericLength ∘ normalizePlaintext → l)
    | isPerfectSquare l = l
    | otherwise         = head $ dropWhile ((<l) ∘ (↑2)) [1..]
    where
      isPerfectSquare i = (floor ∘ sqrt) (fromIntegral i) ≡ i
