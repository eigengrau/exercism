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
{-# LANGUAGE ScopedTypeVariables #-}

module Main (mkDeque, push, pop, shift, unshift, main) where


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
import Data.Maybe
import Text.Printf
import Criterion.Main
import Control.DeepSeq


data Deque α = Deque {
      frontLen   ∷ Int,
      dequeFront ∷ [α],
      backLen    ∷ Int,
      dequeBack  ∷ [α]
    } deriving (Show, Generic, Typeable, NFData)

instance Eq α ⇒ Eq (Deque α) where

    Deque _ f₁ _ b₁ == Deque _ f₂ _ b₂ =
        f₁ ⧺ reverse b₁ ≡ f₂ ⧺ reverse b₂

instance Ord α ⇒ Ord (Deque α) where

    Deque _ f₁ _ b₁ `compare` Deque _ f₂ _ b₂ =
        (f₁ ⧺ reverse b₁) `compare` (f₂ ⧺ reverse b₂)

-- instance NFData α ⇒ NFData (Deque α) where
--     rnf (Deque f front b back) = 

instance (Arbitrary α, Show α) ⇒ Arbitrary (Deque α) where

    arbitrary = do
                                       -- We’re just using user-facing API to
                                       -- create arbitrary lists. E. g., this
                                       -- ascertains that the generates deques
                                       -- are in various states of imbalance.

      adds  ← listOf $ do
                  fun ← elements [pushʹ, unshiftʹ]
                  val ← arbitrary
                  return (`fun` val)

      removes ← listOf $ do
                  fun ← elements [popʹ, shiftʹ]
                  let funʹ deque = maybe deque fst (fun deque)
                  return funʹ

      ops ← shuffle (adds ⧺ removes)

      let deque = foldl (flip ($)) empty ops
      return deque

    shrink = fmap (rebalance ∘ correctLength) ∘ genericShrink
        where
          correctLength (Deque _ front _ back) =
              Deque (length front) front (length back) back

-- | Length.
dequeLength ∷ Deque α → Int
dequeLength (Deque f _ b _) = f + b

-- | Conversion.
dequeToList ∷ Deque α → [α]
dequeToList (Deque _ f _ b) = f ⧺ (reverse b)


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

peekHead ∷ Deque α → Maybe α
peekHead (Deque _ (x : xs) _ _) = Just x
peekHead (Deque _ [] _ [x])     = Just x
peekHead _                      = Nothing

peekLast ∷ Deque α → Maybe α
peekLast (Deque _ _ _ (x : xs)) = Just x
peekLast (Deque _ [x] _ [])     = Just x
peekLast _                      = Nothing

reverseDeque ∷ Deque α → Deque α
reverseDeque (Deque f front l back) = Deque l back f front

-- | Rebalance a deque when necessary.
rebalance ∷ Deque α → Deque α
rebalance deque@(Deque frontLen front backLen back)

    | frontLen > 2 × backLen =

--        "Rebalance" `traceShow`

          let (frontʹ, imbalance) = splitAt smaller front
              backʹ = back ⧺ reverse imbalance
          in Deque smaller frontʹ larger backʹ

    | backLen > 2 × frontLen =

--        "Rebalance" `traceShow`

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
    "shift"   `fun1` (fmap snd ∘ shiftʹ ∷ Deque A → Maybe A),

    "fromList" `fun1` (mkDequeʹ ∷ [A] → Deque A),
    "toList"   `fun1` (dequeToList ∷ Deque A → [A]),

    "empty" `fun0` (empty ∷ Deque A),

    "length" `fun1` (dequeLength ∷ Deque A → Int),

    "rebalance" `fun1` (rebalance ∷ Deque A → Deque A),

    "head" `fun1` (peekHead ∷ Deque A → Maybe A),
    "last" `fun1` (peekLast ∷ Deque A → Maybe A),

    "reverse"  `fun1` (reverseDeque ∷ Deque A → Deque A),
    background $ "reverseL" `fun1` (reverse      ∷ [A] → [A])
  ]


runTests ∷ IO ()
runTests = quickCheckWith settings (conjoin props)

    where
      settings = stdArgs { maxSuccess = 1000 }
      props    = [
          label "seq"          $ property prop_seq,
          label "popPush"      $ property prop_popPush,
          label "shiftunshift" $ property prop_shiftUnshift,
          label "conversionLD" $ property prop_conversion_ld,
          label "conversionDL" $ property prop_conversion_dl,
          label "unshiftDL"    $ property prop_unshift_dl,
          label "unshiftLD"    $ property prop_unshift_ld,
          label "pushLD"       $ property prop_push_ld,
          label "pushDL"       $ property prop_push_dl,
          label "rebalanceID"  $ property prop_balance_id,
          label "frontLen"     $ property prop_frontlen,
          label "backLen"      $ property prop_backlen
        ]

      prop_seq deque (x∷A) y =
          unshiftʹ (pushʹ deque x) y ≡ pushʹ (unshiftʹ deque y) x

      prop_popPush      deque (x∷A) = popʹ   (pushʹ    deque x) ≡ Just (deque,x)
      prop_shiftUnshift deque (x∷A) = shiftʹ (unshiftʹ deque x) ≡ Just (deque,x)

      prop_conversion_ld (list∷[A]) =
          dequeToList (mkDequeʹ list) ≡ list
      prop_conversion_dl (deque∷Deque A) =
          mkDequeʹ (dequeToList deque) ≡ deque

      prop_unshift_dl d (x∷A) = x : dequeToList d   ≡ dequeToList (unshiftʹ d x)
      prop_push_dl    d (x∷A) = dequeToList d ⧺ [x] ≡ dequeToList (pushʹ    d x)

      prop_unshift_ld l (x∷A) = mkDequeʹ (x : l)   ≡ unshiftʹ (mkDequeʹ l) x
      prop_push_ld    l (x∷A) = mkDequeʹ (l ⧺ [x]) ≡ pushʹ    (mkDequeʹ l) x

      prop_balance_id (d ∷ Deque A) = d ≡ rebalance d

      prop_frontlen (Deque _ (front ∷ [A]) _ back) =
          length front > 0 ∨ length back ≤ 1
      prop_backlen (Deque _ front _ (back ∷ [A])) =
          length back > 0 ∨ length front ≤ 1

benchmarks ∷ IO ()
benchmarks = do
--  deque30  ← generate (arbitrary `suchThat` ((>30)  ∘ dequeLength))
--  deque300 ← generate (arbitrary `suchThat` ((>300) ∘ dequeLength))
  deques ← generate (listOf arbitrary `suchThat` ((>10) ∘ length))
  let op deque = pushʹ deque 5
  defaultMain [ bgroup "deques"
                [ bench "wip" $ nf op d
                      | d ← take 5 (deques ∷ [Deque Int]) ]
               ]
  return ()

    where
      mkDeque len = mkDequeʹ [1..len]
      doOps = generate $ do
               adds  ← listOf $ do
                         fun ← elements [pushʹ, unshiftʹ]
                         val ← (arbitrary ∷ Gen Int)
                         return (`fun` val)

               removes ← listOf $ do
                           fun ← elements [popʹ, shiftʹ]
                           let funʹ deque = maybe deque fst (fun deque)
                           return funʹ

               ops ← shuffle (adds ⧺ removes)
               let deque = foldl (flip ($)) empty ops
               return ()

main ∷ IO ()
main = benchmarks
