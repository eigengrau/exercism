{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE UnicodeSyntax         #-}
-- {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

import           Data.Reflection
import           Prelude.Unicode
import Debug.Trace
import Data.Singletons
import Data.Singletons.TH
import GHC.TypeLits
import Data.Singletons.TypeRepStar
import Data.Singletons.Decide
import Data.Type.Equality

singletons [d|
   data Numb = Sg | Pl deriving (Show,Eq)
 |]

data VBar (α∷Numb) (β∷Nat) = V String
--data VBar (α∷Numb) (β∷Nat) =  ∀χ . ((KindOf χ ∷ KProxy α)) ⇒ V χ String

instance (SingI α, β ~ χ, KnownNat χ) ⇒ Show (VBar α β) where
    show (V  s) = "V " ⧺ show s ⧺ " " ⧺
                   (show ∘ demote) (Proxy ∷ Proxy α) ⧺ " " ⧺
                   (show ∘ natVal) (Proxy ∷ Proxy χ)

demote ∷ ∀(α∷k) . (SingI α, SingKind ('KProxy ∷ KProxy k))
         ⇒ Proxy α → Demote α
demote _ = fromSing (sing ∷ Sing α)





data Number a = Number a deriving Show
data Case   a = Case   a deriving Show

data NP a b = NP (Number a) (Case b) deriving Show
data VP a b = VP (Number a) (Case b) deriving Show
data S a b = S (NP a b) (VP a b) deriving Show


data MySg = MySg deriving Show
data MyPl = MyPl deriving Show

-- λ> S (NP (Number MyPl) (Case 1)) (VP (Number MyPl) (Case 1))
--     S (NP (Number MyPl) (Case 1)) (VP (Number MyPl) (Case 1))
-- λ> S (NP (Number MyPl) (Case 1)) (VP (Number MySg) (Case 1))

-- <interactive>:103:43:
--     Couldn't match expected type ‘MyPl’ with actual type ‘MySg’
--     In the first argument of ‘Number’, namely ‘MySg’
--     In the first argument of ‘VP’, namely ‘(Number MySg)’

-- λ> :t NBar (Number SSg)
-- NBar (Number SSg) ∷ NBar (Sing 'Sg)

----------------
-- Using Data.Reflection instead:
-- data Numb = Sg | Pl deriving Show
-- data VBar (a::Numb) = V String
-- instance Reifies Sg Numb where
--     reflect a = Sg
-- instance Reifies Pl Numb where
--     reflect a = Pl

-- instance ∀α β . (Reifies α β, Show β)  ⇒ Show (VBar α) where
--     show (V x) = "V " ⧺ show x ⧺ " " ⧺
--                    show (reflect (undefined ∷ a α))

-- λ> V "hi" :: VBar Pl
--     V "hi" Pl




----
--λ> 1 :: ((Demote 'True :== Demote 'False )) ~ 'True => Int
--    1

-- 1 :: ((True :== True)) ~ 'True => Int
