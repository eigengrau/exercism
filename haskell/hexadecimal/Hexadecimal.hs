{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module Hexadecimal (hexToInt) where

import Data.Char
import Prelude.Unicode
import Prelude.Unicode.SR
import Control.Applicative.Unicode


hexToInt ∷ Integral α ⇒ String → α
hexToInt s = maybe 0 sum values
    where
      values      = zipWith (×) ⦷ pure multipliers ⊛ mapM hexDigit s
      multipliers = reverse $ take (length s) powers16
      powers16    = 1 : map (×16) powers16


hexDigit ∷ Integral α ⇒ Char → Maybe α
hexDigit (toLower → c)
    | c ∈ ['0'..'9'] = Just (fromIntegral $ ord c - ord '0')
    | c ∈ ['a'..'f'] = Just (fromIntegral $ ord c - ord 'a' + 10)
    | otherwise      = Nothing
