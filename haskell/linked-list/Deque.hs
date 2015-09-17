{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Deque (mkDeque, push, pop, shift, unshift) where


import Debug.Trace
import Prelude.Unicode
import Data.IORef
import Control.Monad.Unicode
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Prelude.Unicode.SR
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Data.Typeable
import Test.QuickCheck
import GHC.Generics
import Test.QuickSpec


data Deque α = Deque {
      frontLen   ∷ Int,
      dequeFront ∷ [α],
      backLen    ∷ Int,
      dequeBack  ∷ [α]
    } deriving (Show, Generic, Typeable)

instance Eq α ⇒ Eq (Deque α) where

    Deque _ f₁ _ b₁ == Deque _ f₂ _ b₂ =
        f₁ ⧺ reverse b₁ ≡ f₂ ⧺ reverse b₂

instance Ord α ⇒ Ord (Deque α) where

    Deque _ f₁ _ b₁ `compare` Deque _ f₂ _ b₂ =
        (f₁ ⧺ reverse b₁) `compare` (f₂ ⧺ reverse b₂)

instance Arbitrary α ⇒ Arbitrary (Deque α) where

    arbitrary = do
      front ← arbitrary
      back  ← arbitrary
      return ∘ rebalance $ Deque (length front) front (length back) back

    shrink = fmap (rebalance ∘ correctLength) ∘ genericShrink
        where
          correctLength (Deque _ front _ back) =
              Deque (length front) front (length back) back


-- | The empty Deque.
empty ∷ Deque α
empty = Deque 0 [] 0 []


mkDeque ∷ IO (IORef (Deque α))
mkDeque = newIORef empty

mkDequeʹ ∷ [α] → Deque α
mkDequeʹ l = rebalance $ Deque (length l) l 0 []


-- | Insert value at back.
pushʹ ∷ Deque α → α → Deque α
pushʹ (Deque f front b back) e = rebalance $ Deque f front (succ b) (e : back)


push ∷ IORef (Deque α) → α → IO ()
push ref e = modifyIORef ref (`pushʹ` e)


-- | Insert value at front.
unshiftʹ ∷ Deque α → α → Deque α
unshiftʹ (Deque f front b back) e = rebalance $ Deque (succ f) (e : front) b back


unshift ∷ IORef (Deque α) → α → IO ()
unshift ref e = modifyIORef ref (`unshiftʹ` e)


-- | Remove value at back.
popʹ ∷ Deque α → Maybe (Deque α, α)
popʹ (Deque f front b (x:xs)) = Just (rebalance $ Deque f front (pred b) xs, x)
popʹ (Deque _ [x]   _ []    ) = Just (rebalance $ Deque 0 [] 0 [], x)
popʹ (Deque _ _     _ []    ) = Nothing


pop ∷ IORef (Deque α) → IO (Maybe α)
pop ref = runMaybeT $ do
            deque ← liftIO $ readIORef ref
            (new,e) ← MaybeT (return (popʹ deque))
            liftIO $ writeIORef ref new
            return e


-- | Remove value at front.
shiftʹ ∷ Deque α → Maybe (Deque α, α)
shiftʹ (Deque f (x : xs) b back) = Just (rebalance $ Deque (pred f) xs b back, x)
shiftʹ (Deque _ []       _ [x] ) = Just (rebalance $ Deque 0 [] 0 [], x)
shiftʹ (Deque _ []       _    _) = Nothing


shift ∷ IORef (Deque α) → IO (Maybe α)
shift ref = runMaybeT $ do
              deque ← liftIO $ readIORef ref
              (new,e) ← MaybeT (return (shiftʹ deque))
              liftIO $ writeIORef ref new
              return e


-- | Rebalance a deque when necessary.
rebalance ∷ Deque α → Deque α
rebalance deque@(Deque frontLen front backLen back)

    | frontLen > 2 × backLen =

        "Rebalance" `traceShow`

          let (frontʹ, imbalance) = splitAt smaller front
              backʹ = back ⧺ reverse imbalance
          in Deque smaller frontʹ larger backʹ

    | backLen > 2 × frontLen =

        "Rebalance" `traceShow`

          let frontʹ = front ⧺ reverse imbalance
              (backʹ, imbalance) = splitAt smaller back
          in Deque larger frontʹ smaller backʹ

    | otherwise = deque

    where
      smaller = (frontLen + backLen) `div` 2
      larger  = (frontLen + backLen) - smaller

-- foldl1' (>=>) (replicate 50 (\s -> popʹ s >>= return . fst >>= \r -> (unsafePerformIO (myPrint r)) `seq` return r)) (mkDequeʹ [1..50])  

-- 100,100 … 49,100→75,74 … 36/74→55/55 … 27/55→41/41 … 20/41→31/30 … 14/30→22/22 … 10/22→16/16 … 7/16→12/11 … 5/11→8/8 … 3/8→6/5 … 2/5 → 4/3 … 1/3→2/2 … 0/2→1/1 …
-- 100,100 … 24/100→62/62 … 15/62→19/18 … 3/18→11/10 … 2/10→6/6 … 1/6→4/3 … 0/3→1/2 …

specs ∷ IO ()
specs = quickSpec [
    prelude (undefined ∷ A),

    ["deque"]     `vars` (undefined ∷ Deque A),
    ["x","y","z"] `vars` (undefined ∷ A),

    "Just" `fun1` (Just ∷ A → Maybe A),
    "Just" `fun1` (Just ∷ Deque A → Maybe (Deque A)),

    "push" `fun2` (pushʹ ∷ Deque A → A → Deque A),
    "pop"  `fun1` (popʹ ∷ Deque A → Maybe (Deque A, A)),
    "pop"  `fun1` (fmap fst ∘ popʹ ∷ Deque A → Maybe (Deque A)),
    "pop"  `fun1` (fmap snd ∘ popʹ ∷ Deque A → Maybe A),

    "unshift" `fun2` (unshiftʹ ∷ Deque A → A → Deque A),
    "shift"   `fun1` (fmap fst ∘ shiftʹ ∷ Deque A → Maybe (Deque A)),
    "shift"   `fun1` (fmap snd ∘ shiftʹ ∷ Deque A → Maybe A)
  ]


runTests ∷ IO ()
runTests = quickCheckWith settings (conjoin props)
    where
      settings = stdArgs { maxSuccess = 500 }
      props    = [ property prop_seq,
                   property prop_popPush,
                   property prop_shiftUnshift ]

      prop_seq ∷ Deque A → A → A → Bool
      prop_seq deque x y =
          unshiftʹ (pushʹ deque x) y ≡ pushʹ (unshiftʹ deque y) x

      prop_popPush, prop_shiftUnshift ∷ Deque A → A → Bool
      prop_popPush deque x = popʹ (pushʹ deque x) ≡ Just (deque, x)
      prop_shiftUnshift deque x = shiftʹ (unshiftʹ deque x) ≡ Just (deque, x)
