{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnicodeSyntax         #-}


import Data.Data
import Prelude.Unicode
import Data.Typeable
import           Data.Monoid.Unicode
import           Data.Singletons
import           Data.Singletons.TH
import           Text.ParserCombinators.ReadP


singletons [d|
   data Heads = N | V
     deriving (Show, Data, Typeable, Eq)
   data Empty = Empty
     deriving (Show, Data, Typeable, Eq)
 |]


-- TODO: the whole tree needs to be represented at the type level, so
-- that show instances can be defined. The problem seems to be that,
-- as far as the type system is concerned, the non-dominant head is
-- not represented in this type. Normally, existentials should be
-- represented, but in this case, I was both positing a show instance
-- in the type, and inside the show instance itself (which recursed
-- into the type), which is circular. (Though I wonder why this didn’t
-- terminate with the Node constructor.)
data Phrase head next leaf =
    ∀ headʹ nextʹ wants wantsʹ givesʹ any gives anyʹ .
          ((wants :+: gives) ~ next,
           head  ~ Head (Sing headʹ),  Show (Demote headʹ),
           next  ~ Want (Sing nextʹ),  Show (Demote nextʹ),
           wants ~ Want (Sing wantsʹ), Show (Demote wantsʹ),
           gives ~ Head (Sing givesʹ), Show (Demote givesʹ),
           any   ~ Want (Sing anyʹ),   Show (Demote anyʹ))
    ⇒ PhraseL                                             -- Head-left phrase.
          (Phrase head wants leaf)
          (Phrase gives any leaf)
  | ∀ headʹ nextʹ .
          (head ~ Head (Sing headʹ),
           next ~ Want (Sing nextʹ), SingKind (KindOf nextʹ))
    ⇒ Node head next leaf                -- Lexical projection.

deriving instance Functor     (Phrase α β)
deriving instance Foldable    (Phrase α β)
deriving instance Traversable (Phrase α β)
deriving instance Typeable    (Phrase α β)


-- instance {-# OVERLAPPING #-} (Show λ,
--           Show (Phrase (Head (Sing a)) (Want (Sing b)) λ),
--           Show (Phrase (Head (Sing b)) (Want (Sing c)) λ))
-- --          SingKind (KindOf a), SingI a,
-- --          SingKind (KindOf b), SingI b,
-- --          SingKind (KindOf c), SingI c)
--     ⇒ Show (Phrase (Head (Sing a)) (Want (Sing Nothing)) λ) where

--         show (PhraseL a b) = show a ⧺ show b


instance {-# OVERLAPPING #-}  (Show (Demote α),
          Show (Demote β),
          Show λ,
          SingKind (KindOf α), SingI α,
          SingKind (KindOf β), SingI β)
    ⇒ Show (Phrase (Head (Sing α)) (Want (Sing β)) λ) where

        -- show (PhraseL (a ∷ Phrase (Head (Sing α)) wants λ)
        --               (b ∷ Phrase gives (Want (Sing y)) λ))

        --     = "PhraseL (" ⧺ show a ⧺ ") (" ⧺ show b ⧺ ")"


        show (Node head next leaf) = "Node [" ⧺ show headʹ ⧺ ", " ⧺
                                                show nextʹ ⧺ ", " ⧺
                                                show leaf  ⧺ "]"
            where
              headʹ = fromSing (sing ∷ Sing α)
              nextʹ = fromSing (sing ∷ Sing β)

data Head cat = Head  cat deriving Show
data Want cat = Wants cat deriving Show

filterS ∷ ∀ head1 head2 next leaf.
            (Eq head1,
             head1 ~ Demote head2,
             SingI head2,
             SingKind (KindOf head2),
             Show (Demote head2))
          ⇒ head1
          → Phrase (Head (Sing head2)) (Want (Sing next)) leaf
          → IO (Maybe (Demote head2))
-- filterS q (PhraseL a b) = filterS q a >> filterS q b
filterS q (Node p next leaf) = do

     let x = fromSing (sing ∷ Sing head2)
     if q ≡ x
     then return (Just x)
     else return Nothing

     -- λ> filterS V ((Node (Head SN) (Wants SEmpty) "") :: Phrase (Head (Sing 'N)) (Want (Sing 'Empty)) String
     -- )
     --     Nothing
     -- λ> filterS N ((Node (Head SN) (Wants SEmpty) "") :: Phrase (Head (Sing 'N)) (Want (Sing 'Empty)) String
     -- )
     --     Just N

demote ∷ ∀(α∷k) . (SingI α, SingKind ('KProxy ∷ KProxy k))
         ⇒ Proxy α → Demote α
demote _ = fromSing (sing ∷ Sing α)

-- Unification of percolated attributes is implemented as a closed
-- type family.

type family α :+: β ∷ * where

    -- For now assume a simple model where a subcategorized head is
    -- satisfied by just that kind of head.
    Want (Sing 'N) :+: Head (Sing 'N) = Want (Sing 'Empty)
--    Want (Sing x) :+: Head (Sing x) = Want (Sing 'Empty)
--    Want x :+: Head x = Want (Sing 'Empty)
--    x :+: x = Want (Sing 'Empty)



-- One could also associated the type family to a type class, but the
-- family won’t be closed then (no idea if this would be a big
-- drawback in this domain). The type class might afford other
-- niceties by associating functions to the class.
--
-- class Unifies α β where
--     type α :+: β ∷ *
-- instance (Show α) ⇒ Unifies α α where
--     type α :+: α = Sing 'Empty


-- PhraseL (Node (Head SV) (Wants SN)) (Node (Head SN) (Wants undefined))
--   ∷ Phrase (Head (Sing 'V)) (Want (Sing 'Empty))
-- 1 ∷ ((Want (Sing 'N) :+: Head (Sing 'N)) ~ Want (Sing 'Empty)) => Int


------------------------
-- A parser instance. --
------------------------

np ∷ ReadP (Phrase (Head (Sing 'N)) (Want (Sing 'Empty)) String)
np = return (Node (Head SN) (Wants SEmpty) "hi")

-- Transitive VP.
vp1 ∷ ReadP (Phrase (Head (Sing 'V)) (Want (Sing 'N)) String)
vp1 = return (Node (Head SV) (Wants SN) "hi")

-- Full VP.
vp ∷ ReadP (Phrase (Head (Sing 'V)) (Want (Sing 'Empty)) String)
vp = do
  vp ← vp1
  np ← np
  return (PhraseL vp np)
