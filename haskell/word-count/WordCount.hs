{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module WordCount (wordCount) where


import           Data.Char
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Prelude.Unicode
import           Text.Regex.PCRE (getAllTextMatches, (=~))


wordCount ∷ String → Map String Int
wordCount = foldr addCount Map.empty ∘ tokenize


tokenize ∷ String → [String]
tokenize = getAllTextMatches ∘ (=~ tokenizeRe)
  where
    tokenizeRe = "([[:alpha:]]|[[:digit:]])+"


addCount ∷ String → Map String Int → Map String Int
addCount (map toLower → word) counts =
    Map.insert word newCount counts

  where
    newCount = maybe 1 succ (Map.lookup word counts)
