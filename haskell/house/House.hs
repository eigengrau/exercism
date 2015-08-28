{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}

{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UnicodeSyntax             #-}

module House (rhyme) where

import           Control.Applicative          hiding (many, optional, some)
import           Control.Applicative.Unicode
import           Control.Monad
import           Data.Char
import           Data.Data
import           Data.List
import           Data.Maybe
import           Data.Monoid.Unicode
import           Data.Typeable
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Text.ParserCombinators.ReadP


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

-- A tree with leaves of type λ, where left nodes must be headed by an
-- α, and right nodes must be headed by a β. This doesn’t capture that
-- α and β should be maximal projections in their respective category.
-- The practical effect is that this forbids the construction of
-- parsing rules where the subordinate nodes are not headed by the
-- categories which the superordinate tree demands.

data Tree α λ =

    -- The Typeable constraints are only so that the types of non-head
    -- nodes may be pretty-printed from the Show instance.

    ∀β . (Typeable β) ⇒ NodeL (Tree α λ) (Tree β λ) |   -- Head left.
    ∀β . (Typeable β) ⇒ NodeR (Tree β λ) (Tree α λ) |   -- Head right.
    Leaf λ



--deriving instance Data α ⇒ Data (Tree α String)
deriving instance Functor (Tree α)

--deriving instance Show λ ⇒ Show (Tree α λ)
instance (Show λ, Typeable α) ⇒ Show (Tree α λ) where
    show (NodeL a b) = "NodeL " ⧺ show (typeRep (Proxy ∷ Proxy α)) ⧺
                         "(" ⧺ show a ⧺ "," ⧺ show b ⧺ ")"
    show (NodeR a b) = "NodeR " ⧺ show (typeRep (Proxy ∷ Proxy α)) ⧺
                         "(" ⧺ show a ⧺ "," ⧺ show b ⧺ ")"
    show (Leaf x)    = "Leaf "  ⧺ show (typeRep (Proxy ∷ Proxy α)) ⧺ show x



instance Foldable (Tree α) where
    foldMap f (NodeL a b)    = foldMap f a ⊕ foldMap f b
    foldMap f (NodeR a b)    = foldMap f a ⊕ foldMap f b
    foldMap f (Leaf x)       = f x

instance Traversable (Tree α) where
    traverse f (NodeL a b) = NodeL ⦷ traverse f a ⊛ traverse f b
    traverse f (NodeR a b) = NodeR ⦷ traverse f a ⊛ traverse f b
    traverse f (Leaf x)    = Leaf  ⦷ f x


data N --deriving Typeable -- Nominal projections.
data V --deriving Typeable -- Verbal projections.
data C --deriving Typeable -- Complementizers.
data P

------------------------------------
-- A toy grammar for this domain. --
------------------------------------

sent ∷ ReadP (Tree V [String])
sent = do
  n ← np
  v ← vp
  optional (char '.')
  eof
  return (NodeR n v)

      where

        vp ∷ ReadP (Tree V [String])
        vp = do
          v ← verb
          object ← option Nothing (fmap Just $ pp +++ np)
          return $ case object of
                     Nothing → v
                     Just content → NodeL v content

        pp ∷ ReadP (Tree N [String])
        pp = do
          p ← prep
          n ← np
          return $ NodeL p n

        prep ∷ ReadP (Tree N [String])
        prep = do
          p ← choice (fmap tstring preps)
          return (Leaf [p])

        verb ∷ ReadP (Tree V [String])
        verb = do
          v ← choice (fmap tstring verbs)
          return (Leaf [v])

        np ∷ ReadP (Tree N [String])
        np = do
          t ← many1 (non "that")
          comp ← option Nothing (fmap Just (pp +++ cp))
          return $ case comp of
                     Nothing → Leaf t
                     Just c  → NodeL (Leaf t) c

        cp ∷ ReadP (Tree N [String])
        cp = do
          c ← comp
          subj ← option Nothing (fmap Just np)
          v ← verb -- p ∷ ReadP (Tree V [String])
          object ← option Nothing (fmap Just (pp +++ np))
          let vpʹ = case object of
                       Nothing → v
                       Just o → NodeL v o
          let vpʹʹ = case subj of
                      Nothing → vpʹ
                      Just s  → NodeR s vpʹ
          return $ NodeL c vpʹʹ

        comp ∷ ReadP (Tree N [String])
        comp = do
          c ← choice (fmap tstring comps)
          return (Leaf [c])

        token ∷ ReadP String
        token = do
          t ← munch1 isAlpha
          skipSpaces
          return t

        tstring ∷ String → ReadP String
        tstring s = do
          result ← string s
          skipSpaces
          return result

        non ∷ String → ReadP String
        non s = do
          t ← token
          if t ≡ s
            then pfail
            else return t

        comps ∷ [String]
        comps = words "that"

        preps ∷ [String]
        preps = words "in with to at"

        verbs ∷ [String]
        verbs = words "is belonged kept crowed woke married \
                      \kissed milked tossed worried killed ate lay built"
