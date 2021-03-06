﻿{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Deque where

import           Control.Applicative.Unicode
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Criterion.Main
import           Data.IORef
import qualified Data.List                   as List
import           Data.Typeable
import           GHC.Generics
import           Prelude                     hiding (head, last, length,
                                              reverse)
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Test.QuickCheck
import           Test.QuickSpec


                        --------------------------------------------------------
                        --
                        -- Synopsis
                        --
                        -- Instead of opting for IO and MVars/STM, as suggested
                        -- in the exercise, I thought I might try going for a
                        -- purely functional O(1) deque instead. I came across
                        -- something called a Banker’s deque, and pursued a
                        -- naive implementation based on the underlying idea.
                        -- The gist of it is that while not every operation is
                        -- O(1), the algorithm will guarantee that any
                        -- non-constant operation will ammortize after a fixed
                        -- amount of subsequent operations.
                        --
                        -- For the Banker’s deque, which maintains two lists
                        -- corresponding to the front resp. rear of the deque,
                        -- we occasionally rebalance front and rear. This is a
                        -- linear time operation (linear on the order of the
                        -- degree of imbalance). We reverse part of the rear and
                        -- add it to the front, or vice versa. Such a rebalance
                        -- is triggered when one side of the deque is twice (or
                        -- some other constant factor) the size of the other.
                        --
                        -- E.g., given a 50/100 split between front and rear,
                        -- the next pop would cause 25 items to be moved over to
                        -- the front, yielding a balanced 74/75 split. The
                        -- associated list traversal and reversal consumes 25
                        -- time units. However, if we go on popping from the
                        -- deque, the next rebalance will only trigger after 25
                        -- O(1) pops, so that the rebalance is guaranteed to
                        -- ammortize into constant time.
                        --
                        -- Since my implementation is pretty naive and not a
                        -- cookbook implementation the formalized Banker’s deque
                        -- balancing algorithm, I set up a Criterion benchmark
                        -- to validate if pop/push really run in ammortized
                        -- constant time.


------------
-- Types. --
------------

data Deque α = Deque {
      frontLength ∷ Int,
      front       ∷ [α],
      backLength  ∷ Int,
      back        ∷ [α]
    } deriving (Show, Generic, Typeable, NFData)


instance Eq α ⇒ Eq (Deque α) where

    Deque _ f₁ _ b₁ == Deque _ f₂ _ b₂ =
        f₁ ⧺ List.reverse b₁ ≡ f₂ ⧺ List.reverse b₂

instance Ord α ⇒ Ord (Deque α) where

    Deque _ f₁ _ b₁ `compare` Deque _ f₂ _ b₂ =
        (f₁ ⧺ List.reverse b₁) `compare` (f₂ ⧺ List.reverse b₂)

instance (Arbitrary α, Show α) ⇒ Arbitrary (Deque α) where

    arbitrary = sized dequeOf


----------------------
-- Querying Deques. --
----------------------

length ∷ Deque α → Int
length (Deque f _ b _) = f + b


null ∷ Deque α → Bool
null (Deque 0 _ 0 _) = True
null _               = False


head ∷ Deque α → Maybe α
head (Deque _ (x : _) _ _  ) = Just x
head (Deque _ []      _ [x]) = Just x
head _                       = Nothing


last ∷ Deque α → Maybe α
last (Deque _ _   _ (x : _)) = Just x
last (Deque _ [x] _ []     ) = Just x
last _                       = Nothing


-------------------
-- Construction. --
-------------------

toList ∷ Deque α → [α]
toList (Deque _ f _ b) = f ⧺ List.reverse b


fromList ∷ [α] → Deque α
fromList l = rebalance $ Deque (List.length l) l 0 []


empty ∷ Deque α
empty = Deque 0 [] 0 []


-------------
-- Update. --
-------------

-- | Insert value at rear/back.
unshiftʹ, pushʹ ∷ Deque α → α → Deque α
pushʹ    (Deque f front b back) e = rebalance $ Deque f front (succ b) (e : back)
unshiftʹ (Deque f front b back) e = rebalance $ Deque (succ f) (e : front) b back


-- | Remove value from the rear.
popʹ ∷ Deque α → Maybe (Deque α, α)
popʹ (Deque f front b (x:xs)) = Just (rebalance $ Deque f front (pred b) xs, x)
popʹ (Deque _ [x]   _ []    ) = Just (rebalance $ Deque 0 []    0        [], x)
popʹ (Deque _ _     _ []    ) = Nothing


-- | Remove value from the front.
shiftʹ ∷ Deque α → Maybe (Deque α, α)
shiftʹ (Deque f (x : xs) b back) = Just (rebalance $ Deque (pred f) xs b back, x)
shiftʹ (Deque _ []       _ [x] ) = Just (rebalance $ Deque 0        [] 0 [],   x)
shiftʹ (Deque _ []       _ _   ) = Nothing


-- | O(1) reversal.
reverse ∷ Deque α → Deque α
reverse (Deque f front l back) = Deque l back f front


-- | Rebalance a deque when necessary.
rebalance ∷ Deque α → Deque α
rebalance deque@(Deque frontLength front backLength back)

                              -- Balancing takes time linear on the order of the
                              -- degree of imbalance. For a 4/10 split, 3 units
                              -- must be moved over to the left, which means 3
                              -- constructors must be traversed in the right
                              -- list in order to split it. However, with a
                              -- factor of 2 triggering the rebalance, the next
                              -- rebalance will only occur on 3/7, so that the
                              -- benefit of this 3 time-unit operation lasts
                              -- over a span of 3 time-units, making the
                              -- operation ammortized constant time.

    | frontLength > factor × backLength =

        let (frontʹ, imbalance) = splitAt smaller front
            backʹ = back ⧺ List.reverse imbalance
        in Deque smaller frontʹ larger backʹ

    | backLength > factor × frontLength =

        let (backʹ, imbalance) = splitAt smaller back
            frontʹ = front ⧺ List.reverse imbalance
        in Deque larger frontʹ smaller backʹ

    | otherwise = deque

    where
      factor  = 2
      smaller = (frontLength + backLength) `div` 2
      larger  = (frontLength + backLength) - smaller


-------------------------
-- Proxy IO interface. --
-------------------------

                                       -- Only needed to fit in with the
                                       -- exercism test suite

shift ∷ IORef (Deque α) → IO (Maybe α)
shift ref = runMaybeT $ do

              deque ← liftIO $ readIORef ref
              (dequeʹ,item) ← MaybeT (return (shiftʹ deque))
              liftIO $ writeIORef ref dequeʹ
              return item


pop ∷ IORef (Deque α) → IO (Maybe α)
pop ref = runMaybeT $ do

            deque ← liftIO $ readIORef ref
            (dequeʹ,item) ← MaybeT (return (popʹ deque))
            liftIO $ writeIORef ref dequeʹ
            return item


mkDeque ∷ IO (IORef (Deque α))
mkDeque = newIORef empty


push ∷ IORef (Deque α) → α → IO ()
push ref e = modifyIORef ref (`pushʹ` e)


unshift ∷ IORef (Deque α) → α → IO ()
unshift ref e = modifyIORef ref (`unshiftʹ` e)


--------------
-- Utility. --
--------------

-- | Generate an arbitrary Deque of fixed size.
dequeOf ∷ (Arbitrary α, Show α) ⇒ Int → Gen (Deque α)
dequeOf n = do
                                       -- We’re just using user-facing API here.
                                       -- Inter alia, this ascertains that the
                                       -- generated deques are in various states
                                       -- of imbalance.

      adds    ← listOf doAdd
      removes ← listOf doRemove
      ops     ← shuffle (adds ⧺ removes)
      let deque = foldl (flip ($)) empty ops

                                      -- After random pushing & popping, we now
                                      -- make sure the deque we return has the
                                      -- desired size.

      case compare (length deque) n of
        EQ → return deque
        LT → iterateUntilM ((≥n) ∘ length) (\d → doAdd    ⊛ pure d) deque
        GT → iterateUntilM ((≤n) ∘ length) (\d → doRemove ⊛ pure d) deque

    where
      doAdd = do
        fun ← elements [pushʹ, unshiftʹ]
        val ← arbitrary
        return (`fun` val)

      doRemove = do
        fun ← elements [popʹ, shiftʹ]
        let funʹ deque = maybe deque fst (fun deque)
        return funʹ


----------------
-- QuickSpec. --
----------------

runSpecs ∷ IO ()
runSpecs = quickSpec [

    prelude (undefined ∷ A),

    ["deque"] `vars` (undefined ∷ Deque A),

                                       -- The prelude adds variables like x ∷ A
                                       -- and xs ∷ [A], so these don’t have to
                                       -- be defined here.

    "Just" `fun1` (Just ∷ A → Maybe A),
    "Just" `fun1` (Just ∷ Deque A → Maybe (Deque A)),

    "push" `fun2` (pushʹ           ∷ Deque A → A → Deque A),
    "pop"  `fun1` (popʹ            ∷ Deque A → Maybe (Deque A, A)),
    "pop"  `fun1` (fmap fst ∘ popʹ ∷ Deque A → Maybe (Deque A)),
    "pop"  `fun1` (fmap snd ∘ popʹ ∷ Deque A → Maybe A),

    "unshift" `fun2` (unshiftʹ          ∷ Deque A → A → Deque A),
    "shift"   `fun1` (fmap fst ∘ shiftʹ ∷ Deque A → Maybe (Deque A)),
    "shift"   `fun1` (fmap snd ∘ shiftʹ ∷ Deque A → Maybe A),

    "fromList" `fun1` (fromList ∷ [A] → Deque A),
    "toList"   `fun1` (toList   ∷ Deque A → [A]),

    "empty" `fun0` (empty ∷ Deque A),

    "length" `fun1` (length ∷ Deque A → Int),

    "rebalance" `fun1` (rebalance ∷ Deque A → Deque A),

    "head" `fun1` (head ∷ Deque A → Maybe A),
    "last" `fun1` (last ∷ Deque A → Maybe A),

    "reverse"  `fun1` (reverse ∷ Deque A → Deque A),
    background $ "reverseL" `fun1` (List.reverse ∷ [A] → [A])
  ]


-----------------
-- QuickCheck. --
-----------------

runTests ∷ IO ()
runTests = quickCheckWith settings (conjoin props)

    where
      settings = stdArgs { maxSuccess = 1000 }
      props    = [
          label "seq"          $ property prop_seq,
          label "popPush"      $ property prop_popPush,
          label "shiftUnshift" $ property prop_shiftUnshift,
          label "conversionLD" $ property prop_conversion_ld,
          label "conversionDL" $ property prop_conversion_dl,
          label "unshiftDL"    $ property prop_unshift_dl,
          label "unshiftLD"    $ property prop_unshift_ld,
          label "pushLD"       $ property prop_push_ld,
          label "pushDL"       $ property prop_push_dl,
          label "rebalanceID"  $ property prop_balance_id,
          label "frontLength"  $ property prop_frontlen,
          label "backLength"   $ property prop_backlen
        ]

      prop_seq deque (x∷A) y =
          unshiftʹ (pushʹ deque x) y ≡ pushʹ (unshiftʹ deque y) x

      prop_popPush      deque (x∷A) = popʹ   (pushʹ    deque x) ≡ Just (deque,x)
      prop_shiftUnshift deque (x∷A) = shiftʹ (unshiftʹ deque x) ≡ Just (deque,x)

      prop_conversion_ld (list∷[A])      = toList   (fromList list) ≡ list
      prop_conversion_dl (deque∷Deque A) = fromList (toList deque)  ≡ deque

      prop_unshift_dl d (x∷A) = x : toList d   ≡ toList (unshiftʹ d x)
      prop_push_dl    d (x∷A) = toList d ⧺ [x] ≡ toList (pushʹ    d x)

      prop_unshift_ld l (x∷A) = fromList (x : l)   ≡ unshiftʹ (fromList l) x
      prop_push_ld    l (x∷A) = fromList (l ⧺ [x]) ≡ pushʹ    (fromList l) x

      prop_balance_id (d ∷ Deque A) = d ≡ rebalance d

      prop_frontlen (Deque _ (front ∷ [A]) _ back) =
          List.length front > 0 ∨ List.length back  ≤ 1
      prop_backlen (Deque _ front _ (back ∷ [A])) =
          List.length back  > 0 ∨ List.length front ≤ 1


----------------
-- Criterion. --
----------------

benchmarks ∷ IO ()
benchmarks = do
                             -- Theorem: A deque can be popped/shifted from or
                             -- unshifted/pushed to in amortzide constant time.
                             -- In order to test this in a way that is sensitive
                             -- to ammortization, we test the following two
                             -- corrolaries.
                             --
                             -- 1. A deque can be reduced to length 0 by popping
                             -- from it, in linear time.
                             --
                             -- 2. A deque can be expanded to duplicate length
                             -- via pushing, in linear time.

  defaultMain [
      bgroup "pop"  $ fmap (\d → bench (show $ length d) (nf consume d)) deques,
      bgroup "push" $ fmap (\d → bench (show $ length d) (nf double  d)) deques
    ]

  return ()

      where
        deques ∷ [Deque Int]
        deques = force $ fmap (\len → fromList [1..len]) [2↑x | x ← [10..20]]

        consume      = until ((≡0)            ∘ length) popMaybe
        double deque = until ((≡2×origLength) ∘ length) (`pushʹ` 1) deque
            where origLength = length deque

        popMaybe d = maybe d fst (popʹ d)


main ∷ IO ()
main = benchmarks


test ∷ Int → [α] → ([α], [α])
test n l = (take n l, drop n l)

intTest2 ∷ Int
intTest2 = 5

{-# NOINLINE test2 #-}
{-# RULES "test2/int" test2 = intTest2 #-}
test2 ∷ Num α ⇒ α
test2 = 1

test3 x = x



-- Factor 2
-- 100,100 … 49,100→75,74 … 36/74→55/55 … 27/55→41/41 … 20/41→31/30 … 14/30→22/22 … 10/22→16/16 … 7/16→12/11 … 5/11→8/8 … 3/8→6/5 … 2/5 → 4/3 … 1/3→2/2 … 0/2→1/1 …
-- Factor 4
-- 100,100 … 24/100→62/62 … 15/62→19/18 … 3/18→11/10 … 2/10→6/6 … 1/6→4/3 … 0/3→1/2 …
