{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module Atbash (encode) where

import           Control.Monad
import           Control.Monad.Unicode
import           Data.Char
import           Data.List.Split             (chunksOf, wordsBy)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Prelude.Unicode


type Alphabet = String

data EncodingMode = ASCII | Unicode


encode ∷ String → String
encode = encodeʹ latin ASCII


-- Generalized encoding for arbitrary alphabets.
encodeʹ ∷ Alphabet → EncodingMode → String → String
encodeʹ alphabet encodingMode (map toLower → input) =
  (unwords ∘ chunksOf 5) cipherStream

    where
      cipherStream =
          encodeWord alphabet encodingMode =≪
              wordsBy (not ∘ isAlphaNum) input


encodeWord ∷ Alphabet → EncodingMode → String → String
encodeWord alphabet encodingMode word = do

  char ← word
  let codeChar = encodeChar alphabet char

  case encodingMode of                                -- Could this be prettier?
    ASCII   → guard (isAscii char)
    Unicode → return ()

  if not (isLetter char)
  then return char
  else maybe (encodingError char) return codeChar

      where
        encodingError c = error ("encodeWord: Invalid input " ⧺ show c)


encodeChar ∷ Alphabet → Char → Maybe Char
encodeChar alphabet char = Map.lookup char (codeBook alphabet)


-- The substitution map between codepoints, given some alphabet.
codeBook ∷ Alphabet → Map Char Char
codeBook alphabet = Map.fromList (zip alphabet cipherbet)
    where cipherbet = reverse alphabet


latin   ∷ String
latin    = ['a' .. 'z']

russian ∷ String                                                  -- Spies only.
russian = "абвгдеёжзийклмнопрстуфхцчшщъыьэюя"
