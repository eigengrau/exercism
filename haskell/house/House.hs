{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ViewPatterns                 #-}
{-# LANGUAGE RecursiveDo                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE UnicodeSyntax             #-}

-- TODO: A monad that logs (via named rules) exactly which rule
-- triggered some subtree.

module House (rhyme) where

import           Control.Applicative          hiding (many, optional, some)
import           Control.Applicative.Unicode
import           Control.Monad
import           Control.Monad.Unicode
import           Data.Generic.Diff hiding (string)
import qualified Data.Generic.Diff as Diff
import           Data.Char
import           Data.Data
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.Monoid.Unicode
import           Data.Singletons
import           Data.Singletons.TH
import           Debug.Trace
import           GHC.Generics
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Text.ParserCombinators.ReadP
import  Text.Printf
import Data.Singletons.Prelude.Eq


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

singletons [d|
    data Head = Det | N | P | V | I | Coord | Subord | Adj | Adv
        deriving (Show, Eq)
 |]


data XP (head∷Head) = XP (XBar head)
                    | XPₛ Specifier (XBar head)
                    | XPₐ (XP head) (X 'Coord) (XP head)

data XBar (head∷Head) = XBar (X head)
                      | XBarₘ (X head) [Complement]
                      | XBarₐ (XBar head) Adjunct
                      | XBarₐʹ Adjunct (XBar head)

data X (head∷Head) = X String

data Complement =
    ∀ (head∷Head) . (SingI head, Show (Demote head)) ⇒ Complement₂ (XP   head)
  | ∀ (head∷Head) . (SingI head, Show (Demote head)) ⇒ Complement₁ (XBar head)
  | ∀ (head∷Head) . (SingI head, Show (Demote head)) ⇒ Complement₀ (X    head)

data Specifier =
    ∀ (head∷Head) . (SingI head, Show (Demote head)) ⇒ Specifier₂ (XP   head)
  | ∀ (head∷Head) . (SingI head, Show (Demote head)) ⇒ Specifier₁ (XBar head)
  | ∀ (head∷Head) . (SingI head, Show (Demote head)) ⇒ Specifier₀ (X    head)

data Adjunct =
    ∀ (head∷Head) . (SingI head, Show (Demote head)) ⇒ Adjunct₂ (XP   head)
  | ∀ (head∷Head) . (SingI head, Show (Demote head)) ⇒ Adjunct₁ (XBar head)
  | ∀ (head∷Head) . (SingI head, Show (Demote head)) ⇒ Adjunct₀ (X    head)

instance Show Adjunct  where
    show (Adjunct₂ x) = show x
    show (Adjunct₁ x) = show x
    show (Adjunct₀ x) = show x

instance Show Complement where
    show (Complement₂ x) = show x
    show (Complement₁ x) = show x
    show (Complement₀ x) = show x

instance Show Specifier where
    show (Specifier₂ x) = show x
    show (Specifier₁ x) = show x
    show (Specifier₀ x) = show x

instance (SingI head, Show (Demote head)) ⇒ Show (XP head) where
    show (XP  head)      = printf "[ %sP %s ] "
                             (show $ fromSing (sing ∷ Sing head))
                             (show head)
    show (XPₛ spec head) = printf "[%sP [ %s ][ %s ]]"
                             (show $ fromSing (sing ∷ Sing head))
                             (show spec) (show head)
    show (XPₐ l j r)     = printf "[%sP [ %s ] %s [ %s ]]"
                             (show $ fromSing (sing ∷ Sing head))
                             (show l) (show j) (show r)

instance (SingI head, Show (Demote head)) ⇒ Show (XBar head) where
    show (XBar   head)       = printf "[%sˈ %s ]"
                                 (show $ fromSing (sing ∷ Sing head))
                                 (show head)
    show (XBarₘ  head comps) = printf "[%sˈ %s [ %s ]]"
                               (show $ fromSing (sing ∷ Sing head))
                               (show head) (comps ≫= (⧺" ") ∘ show)
    show (XBarₐ  head adj)   = printf "[%sˈ %s [ %s ]]"
                                 (show $ fromSing (sing ∷ Sing head))
                                 (show head) (show adj)
    show (XBarₐʹ adj head)   = printf "[%sˈ [ %s ] %s]"
                                 (show $ fromSing (sing ∷ Sing head))
                                 (show adj) (show head)

instance (SingI head, Show (Demote head)) ⇒ Show (X head) where
    show (X s) = printf "[ %s \"%s\" ]" (show $ fromSing (sing ∷ Sing head)) s


------------------
-- Query trees. --
------------------

{-
class FilterHead α where
    type Result α ∷ Head
    filterHead ∷ Demote τ → α → [XP τ]

instance SingI α ⇒ FilterHead (XP (α∷Head)) where
    type Result (XP α) = α
    filterHead query here@(XP head) = if null below
                                      then if query ≡ fromSing (sing ∷ Sing α)
                                           then [here]
                                           else []
                                      else below
        where
          below = filterHead query head

    filterHead query here@(XPₛ spec head) = if null below
                                            then if query ≡ fromSing (sing ∷ Sing α)
                                                 then [here]
                                                 else []
                                            else below
        where
          below = filterHead query head

    filterHead query here@(XPₐ l _ r) = if null below
                                        then if query ≡ fromSing (sing ∷ Sing α)
                                             then [here]
                                             else []
                                        else below
        where below = filterHead query l ⧺ filterHead query r


instance FilterHead (XBar (α∷Head)) where
    filterHead query (XBar head) = []
    filterHead query (XBarₘ head comps) = filterHead query comps
    filterHead query (XBarₐ head adj)   = filterHead query adj
    filterHead query (XBarₐʹ adj head)  = filterHead query adj
instance FilterHead Specifier τ where
    filterHead query (Specifier₂ head) = filterHead query head
    filterHead query (Specifier₁ head) = filterHead query head
    filterHead query (Specifier₀ head) = []
instance FilterHead Adjunct τ where
    filterHead query (Adjunct₂ head) = filterHead query head
    filterHead query (Adjunct₁ head) = filterHead query head
    filterHead query (Adjunct₀ head) = []
instance FilterHead Complement τ where
    filterHead query (Complement₂ head) = filterHead query head
    filterHead query (Complement₁ head) = filterHead query head
    filterHead query (Complement₀ head) = []
instance FilterHead α τ ⇒ FilterHead [α] τ where
    filterHead query = (≫= filterHead query) -}


----------------------
-- Parsing helpers. --
----------------------

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


non ∷ ReadP α → ReadP ()
non unwanted = do
  result ← readP_to_S unwanted ⦷ look
  guard (null result)


runParser ∷ ReadP α → ReadP α
runParser parser = do
  parse ← parser
  eof
  return parse


-------------------
-- Coordination. --
-------------------

type CoordP   = XP 'Coord
type CoordBar = XBar 'Coord
type CoordLex = X 'Coord

coord ∷ ReadP CoordLex
coord =  X ⦷ choice (tstring ⦷ coords)
    where coords = words "and or"


------------------------
-- Tense projections. --
------------------------

type IP   = XP 'I
type IBar = XBar 'I
type ILex = X 'I

ip ∷ ReadP IP
ip = do
  spec  ← (Just ∘ Specifier₂ ⦷ np) ⧻ return Nothing
  ihead ← ibar
  return $ maybe (XPₛ (Specifier₀ (X "(trace)" ∷ ILex)) ihead)
             (`XPₛ` ihead) spec

ibar ∷ ReadP IBar
ibar = do
  -- This always leaves the specifier and adjunct empty.
  complement ← vp
  return $ XBarₘ tense [Complement₂ complement]

tense ∷ ILex
tense = X "(tense)"


-------------------------
-- Verbal projections. --
-------------------------

type VP   = XP 'V
type VBar = XBar 'V
type VLex = X 'V


vp ∷ ReadP VP
vp = do
  -- This always leaves the specifier position empty.
  vhead ← vbar
  return $ XP vhead


vbar ∷ ReadP VBar
vbar = do
  vhead ← verb
  complement ← option Nothing (Just ⦷ (Complement₂ ⦷ np) ⧻
                                       (Complement₂ ⦷ cp) ⧻
                                       (Complement₂ ⦷ pp))
  return $ maybe (XBar vhead) (\comp → XBarₘ vhead [comp]) complement


verb ∷ ReadP VLex
verb = X ⦷ choice (tstring ⦷ verbs)
    where verbs = words "is belonged kept crowed woke married \
                        \kissed milked tossed worried killed ate lay built"


--------------------------
-- Nominal projections. --
--------------------------

type NP   = XP N
type NBar = XBar N
type NLex = X N


np ∷ ReadP NP
np = np ⧻ nps
    where
      np = do
        spec ← option Nothing (fmap (Just ∘ Specifier₀) det)
        head ← nbar
        return $ maybe (XP head) (`XPₛ` head) spec

      nps = do
        l ← np
        c ← coord
        r ← np ⧻ nps
        return $ XPₐ l c r


nbar ∷ ReadP NBar
nbar = withAdj ⧻ nbarʹ
    where
      nbarʹ = do
        nhead ← noun
        complement₁ ← option [] $ (pure ∘ Complement₂ ⦷ adjp) +≫
                                    (pure ∘ Complement₂ ⦷ pp)
        complement₂ ← option [] (pure ∘ Complement₂ ⦷ cp)
        let complement = complement₁ ⧺ complement₂

                          -- Limit how many complements can appear in
                          -- which sequence, because otherwise the
                          -- search space blows up too much.

        return $
          if null complement
            then XBar nhead
            else XBarₘ nhead complement

      withAdj ∷ ReadP NBar
      withAdj = do
        adjunct ← Adjunct₂ ⦷ adjp
        nhead ← nbar
        return $ XBarₐʹ adjunct nhead


noun ∷ ReadP NLex
noun = non ((Complement₀ ⦷ det) ⧻
            (Complement₀ ⦷ adj) ⧻
            (Complement₀ ⦷ prep) ⧻
            (Complement₀ ⦷ adv) ⧻
            (Complement₀ ⦷ verb) ⧻
            (Complement₀ ⦷ conj) ⧻
            (Complement₀ ⦷ coord)) ≫ X ⦷ token
  -- For now assume that a noun is negatively defined by the other
  -- word classes.


-----------------------------
-- Adjectival projections. --
-----------------------------

type AdjP   = XP 'Adj
type AdjBar = XBar 'Adj
type AdjLex = X 'Adj

adjp ∷ ReadP AdjP
adjp = adjpʹ ⧻ adjps

    where
      adjpʹ = do
        spec ← option Nothing (Just ∘ Specifier₀ ⦷ adv)
        adjHead ← adjBar
        return $ maybe (XP adjHead) (`XPₛ` adjHead) spec

      adjps = do
        l ← adjpʹ
        c ← coord
        r ← adjpʹ ⧻ adjps
        return $ XPₐ l c r

adjBar ∷ ReadP AdjBar
adjBar = do
  adjHead ← adj
  complement ← option Nothing (Just ∘ Complement₂ ⦷ np)
  return $ maybe (XBar adjHead) (\comp → XBarₘ adjHead [comp]) complement

adj ∷ ReadP AdjLex
adj = X ⦷ choice (tstring ⦷ adjs)
    where adjs = words "sowing shaven shorn tattered torn forlorn crumpled"


----------------------------
-- Adverbial projections. --
----------------------------

type AdvP   = XP 'Adv
type AdvBar = XBar 'Adv
type AdvLex = X 'Adv

adv ∷ ReadP AdvLex
adv = X ⦷ choice (tstring ⦷ advs)
    where advs = words "all"

--------------------------------
-- Prepositional projections. --
--------------------------------

type PP   = XP 'P
type PBar = XBar 'P
type PLex = X 'P


pp ∷ ReadP (XP 'P)
pp = do
  -- This always leaves the AP in specifier position empty.
  p ← prep
  arg ← np
  return $ XP (XBarₘ p [Complement₂ arg])


prep ∷ ReadP PLex
prep = X ⦷ choice (tstring ⦷ preps)
    where preps = words "in with to at"


-----------------------------
-- Determiner projections. --
-----------------------------

type DetLex = X Det


det ∷ ReadP (X Det)
det = X ⦷ choice (tstring ⦷ dets)
    where dets = words "a an the some many all his"


---------------------------------
-- Complementizer projections. --
---------------------------------

type CP   = XP 'Subord
type CBar = XBar 'Subord
type CLex = X 'Subord

cp ∷ ReadP CP
cp = do
  -- This leaves the specifier always empty.
  chead ← cbar
  return $ XP chead

cbar ∷ ReadP CBar
cbar = do
  chead ← conj
  arg   ← ip
  return $ XBarₘ chead [Complement₂ arg]

conj ∷ ReadP CLex
conj = X ⦷ choice (tstring ⦷ conjs)
    where conjs = words "that"




------------------

-- comp ∷ ReadP (Phrase String)
-- comp = Leaf C ⦷ choice (tstring ⦷ comps)
--     where comps = words "that"

-- prep ∷ ReadP (Phrase String)
-- prep = Leaf P ⦷ choice (tstring ⦷ preps)
--     where preps = words "in with to at"

-- verb ∷ ReadP (Phrase String)
-- verb = Leaf V ⦷ choice (tstring ⦷ verbs)
--     where verbs = words "is belonged kept crowed woke married \
--                         \kissed milked tossed worried killed ate lay built"

-- adj ∷ ReadP (Phrase String)
-- adj = Leaf A ⦷ choice (tstring ⦷ adjs)
--     where adjs = words "sowing shaven shorn tattered torn forlorn crumpled"



-- conj ∷ ReadP (Phrase String)
-- conj = Leaf Con ⦷ choice (tstring ⦷ conjs)
--     where conjs = words "and or"

(⧻) ∷ ReadP α → ReadP α → ReadP α
(⧻) = (+++)
infixr 5 ⧻

(<⧺) ∷ ReadP α → ReadP α → ReadP α
(<⧺) = (<++)
infixr 5 <⧺

(⧺>) ∷ ReadP α → ReadP α → ReadP α
(⧺>) = flip (<⧺)
infixl 5 ⧺>
