{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module WordCount (wordCount) where

import           Data.Char
import qualified Data.List       as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Prelude.Unicode
import           Text.Regex.PCRE (getAllTextMatches, (=~))


wordCount ∷ String → Map String Int
wordCount = List.foldl' addCount Map.empty ∘ tokenize


tokenize ∷ String → [String]
tokenize = getAllTextMatches ∘ (=~ tokenizeRe)
  where
    tokenizeRe = "([[:alpha:]]|[[:digit:]])+"


addCount ∷ Map String Int → String → Map String Int
addCount counts (map toLower → word) =
    Map.insertWith (+) word 1 counts
