{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE UnicodeSyntax      #-}
{-# LANGUAGE ExistentialQuantification      #-}
{-# LANGUAGE StandaloneDeriving      #-}

module House (rhyme) where

import           Control.Applicative          hiding (many, optional, some)
import           Control.Applicative.Unicode
import           Control.Monad
import           Control.Monad.Unicode
import           Data.Char
import Data.Functor
import           Data.Data
import           Data.List
import           Data.Maybe
import           Data.Monoid.Unicode
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Text.ParserCombinators.ReadP
import GHC.Generics


rhyme = undefined


target ∷ String
target = concat ∘ intersperse " " $ [
    "This is the horse and the hound and the horn",
    "that belonged to the farmer sowing his corn",
    "that kept the rooster that crowed in the morn",
    "that woke the priest all shaven and shorn",
    "that married the man all tattered and torn",
    "that kissed the maiden all forlorn",
    "that milked the cow with the crumpled horn",
    "that tossed the dog",
    "that worried the cat",
    "that killed the rat",
    "that ate the malt",
    "that lay in the house that Jack built."
  ]


------------------------------
-- A toy grammar framework. --
------------------------------

data Phrase λ = Phrase Head (Phrase λ) (Phrase λ)
              | Leaf Head λ
deriving instance Show λ ⇒ Show (Phrase λ)
deriving instance Functor (Phrase)
deriving instance Traversable (Phrase)
deriving instance Foldable (Phrase)
deriving instance Generic (Phrase λ)
deriving instance Data λ ⇒ Data (Phrase λ)

data Head = S | N | V | C | P | A | D | Con deriving (Data, Show)


----------------------------------
-- Generic programming helpers. --
----------------------------------



------------------------------------
-- A toy grammar for this domain. --
------------------------------------

np ∷ ReadP (Phrase String)
np = do
  d ← option Nothing (Just ⦷ det)
  h ← nbar +≫ conjp nbar
  return $ maybe h (\d → Phrase N d h) d

nbar ∷ ReadP (Phrase String)
nbar = do
  a ← option Nothing (Just ⦷ adj)
  h ← noun
  c ← option Nothing (Just ⦷ cp ⧻ pp ⧻ adjp)
  let projection₁ = maybe h (Phrase N h) c
      projection₂ = maybe projection₁ (\a → Phrase N a projection₁) a
  return projection₂

adjp ∷ ReadP (Phrase String)
adjp = do
  d ← option Nothing (Just ⦷ det)
  b ← adjbar
  return $ maybe b (\d → Phrase A d b) d

adjbar ∷ ReadP (Phrase String)
adjbar = adjbarʹ -- +≫ conjp adjbarʹ
    where
      adjbarʹ = do
        a ← adj
        c ← option Nothing (Just ⦷ np ⧻ pp ⧻ conjp np)
        return $ maybe a (\c → Phrase A a c) c

conjp ∷ ReadP (Phrase String) → ReadP (Phrase String)
conjp phrase = do
  p₁ ← phrase
  let h = case p₁ of
            Phrase h _ _ → h
            Leaf h _     → h
  c  ← conj
  p₂ ← phrase +≫ conjp phrase
  return $ Phrase h p₁ (Phrase Con c p₂)

cp ∷ ReadP (Phrase String)
cp = do
  h ← comp
  b ← vbar
  return $ Phrase C h b

s ∷ ReadP (Phrase String)
s = do
  subj ← np +≫ conjp np
  vp ← vbar
  optional (char '.')
  -- eof
  return $ Phrase V subj vp

vbar ∷ ReadP (Phrase String)
vbar = vbar₁ +≫ vbar₂
    where
      vbar₁ = do
        h ← verb
        b ← option Nothing (Just ⦷ np ⧻ conjp np ⧻ pp)
        return $ maybe h (Phrase V h) b

      vbar₂ = do
        b ← np ⧻ conjp np ⧻ pp
        h ← verb
        return (Phrase V b h)

pp ∷ ReadP (Phrase String)
pp = do
  h ← prep
  b ← np
  return (Phrase P h b)

token ∷ ReadP String
token = do
  t ← munch1 isAlpha
  void (munch1 isSpace) ⧻ eof
  skipSpaces
  return t

tstring ∷ String → ReadP String
tstring s = do
  result ← string s
  void (munch1 isSpace) ⧻ eof
  skipSpaces
  return result

non ∷ ReadP a → ReadP ()
non unwanted = do
  result ← readP_to_S unwanted ⦷ look
  guard (null result)

noun ∷ ReadP (Phrase String)
noun = non (comp ⧻ prep ⧻ verb ⧻ det) ≫ Leaf N ⦷ token

comp ∷ ReadP (Phrase String)
comp = Leaf C ⦷ choice (tstring ⦷ comps)
    where comps = words "that"

prep ∷ ReadP (Phrase String)
prep = Leaf P ⦷ choice (tstring ⦷ preps)
    where preps = words "in with to at"

verb ∷ ReadP (Phrase String)
verb = Leaf V ⦷ choice (tstring ⦷ verbs)
    where verbs = words "is belonged kept crowed woke married \
                        \kissed milked tossed worried killed ate lay built"

adj ∷ ReadP (Phrase String)
adj = Leaf A ⦷ choice (tstring ⦷ adjs)
    where adjs = words "sowing shaven shorn tattered torn forlorn crumpled"

det ∷ ReadP (Phrase String)
det = Leaf D ⦷ choice (tstring ⦷ dets)
    where dets = words "a an the some many all his"

conj ∷ ReadP (Phrase String)
conj = Leaf Con ⦷ choice (tstring ⦷ conjs)
    where conjs = words "and or"

(⧻) ∷ ReadP α → ReadP α → ReadP α
(⧻) = (+++)
infixr 5 ⧻

(<⧺) ∷ ReadP α → ReadP α → ReadP α
(<⧺) = (<++)
infixr 5 <⧺
