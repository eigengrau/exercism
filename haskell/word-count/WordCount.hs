{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module WordCount (wordCount) where

import           Data.Char
import qualified Data.List          as List
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import qualified Data.Text          as Text
import qualified Data.Text.ICU      as ICU
import           Prelude.Unicode
import           Prelude.Unicode.SR ((∨∨))
import           Text.Regex.PCRE    (getAllTextMatches, (=~))


wordCount ∷ String → Map String Int
wordCount = List.foldl' addCount Map.empty ∘ tokenize


tokenize ∷ String → [String]
tokenize = tokenizeWithRe


tokenizeWithRe ∷ String → [String]
tokenizeWithRe = getAllTextMatches ∘ (=~ tokenizeRe)
  where
    tokenizeRe = "(\\p{L}|\\p{N})+"


-- Alternative tokenization. Since Unicode characters are not
-- necessarily single Chars, the input has to be normalized into NFC,
-- so that the Data.Char predicates can be reasonably applied.
tokenizeAlternative ∷ String → [String]
tokenizeAlternative = splitTokens ∘ normalize
  where
    normalize   = Text.unpack ∘ ICU.normalize ICU.NFC ∘ Text.pack
    isTokenChar = isNumber ∨∨ isLetter
    splitTokens input@(~(_:cs))
      | null input = []
      | null token = splitTokens cs
      | otherwise  = token : splitTokens remain
      where (token, remain) = List.span isTokenChar input


addCount ∷ Map String Int → String → Map String Int
addCount counts (map toLower → word) =
    Map.insertWith (+) word 1 counts
