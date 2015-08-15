{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module WordCount (wordCount) where


import           Data.Char
import qualified Data.List          as List
import           Data.List.Split    (wordsBy)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import qualified Data.Text          as Text
import qualified Data.Text.ICU      as ICU
import           Prelude.Unicode
import           Prelude.Unicode.SR ((∨∨))


wordCount ∷ String → Map String Int
wordCount = List.foldl' addCount Map.empty ∘ tokenize


-- Since Unicode characters are not necessarily single Chars, the
-- input has to be normalized into NFC, so that the Data.Char
-- predicates can be reasonably applied.
tokenize ∷ String → [String]
tokenize = wordsBy isTokenBoundary ∘ normalize
  where
    normalize = Text.unpack ∘ ICU.normalize ICU.NFC ∘ Text.pack
    isTokenBoundary = not ∘ (isNumber ∨∨ isLetter)


addCount ∷ Map String Int → String → Map String Int
addCount counts (map toLower → word) =
    Map.insertWith (+) word 1 counts
