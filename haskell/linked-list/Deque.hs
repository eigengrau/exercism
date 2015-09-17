{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Deque (mkDeque, push, pop, shift, unshift) where

import Prelude.Unicode
import Data.IORef
import Control.Monad.Unicode
import Control.Monad.Trans
import Control.Monad.Trans.Maybe


data Deque α = Deque {
      frontLen   ∷ Int,
      dequeFront ∷ [α],
      backLen    ∷ Int,
      dequeBack  ∷ [α]
    } deriving Show


-- | The empty Deque.
empty ∷ Deque α
empty = Deque 0 [] 0 []


mkDeque ∷ IO (IORef (Deque α))
mkDeque = newIORef empty


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
popʹ (Deque f _     b []    ) = Nothing


pop ∷ IORef (Deque α) → IO (Maybe α)
pop ref = runMaybeT $ do
            deque ← liftIO $ readIORef ref
            (new,e) ← MaybeT (return (popʹ deque))
            liftIO $ writeIORef ref new
            return e


-- | Remove value at front.
shiftʹ ∷ Deque α → Maybe (Deque α, α)
shiftʹ (Deque f (x : xs) b back) = Just (rebalance $ Deque (pred f) xs b back, x)
shiftʹ (Deque _ []       _    _) = Nothing


shift ∷ IORef (Deque α) → IO (Maybe α)
shift ref = runMaybeT $ do
              deque ← liftIO $ readIORef ref
              (new,e) ← MaybeT (return (shiftʹ deque))
              liftIO $ writeIORef ref new
              return e


-- | Rebalance a deque.
rebalance ∷ Deque α → Deque α
rebalance (Deque 0 []    0 []  ) = Deque 0 [] 0 []
rebalance (Deque f front b back) = Deque f (front ⧺ reverse back) b (reverse front ⧺ back)
