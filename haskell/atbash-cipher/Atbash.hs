{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module Atbash (encode) where

import           Control.Monad
import           Control.Monad.Except
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
  (unwords ∘ chunksOf 5) (either errorʹ id cipherStream)

    where
      cipherStream = sequence ∘ runExceptT $
                       encodeWord alphabet encodingMode =≪
                         lift (wordsBy (not ∘ isAlphaNum) input)

      errorʹ e = error ("encodeʹ: " ⧺ show e)


encodeWord ∷ Alphabet → EncodingMode → String → ExceptT Char [] Char
encodeWord alphabet encodingMode word = do

  char ← lift word
  let codeChar = encodeChar alphabet char

  -- Could this be made prettier somehow?
  case encodingMode of
    ASCII   → lift $ guard (isAscii char)
    Unicode → return ()

  if not (isLetter char)
  then return char
  else maybe (throwError char) return codeChar


encodeChar ∷ Alphabet → Char → Maybe Char
encodeChar alphabet char = Map.lookup char (codeBook alphabet)


-- The substitution map between codepoints, given some alphabet.
codeBook ∷ Alphabet → Map Char Char
codeBook alphabet = Map.fromList (zip alphabet cipherbet)
    where cipherbet = reverse alphabet


latin   ∷ String
latin    = ['a' .. 'z']

russian ∷ String                                                  -- Spies only.
russian = 'ё' : ['а' .. 'я']
