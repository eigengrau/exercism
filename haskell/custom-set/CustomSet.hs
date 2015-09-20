{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE UnicodeSyntax      #-}

module CustomSet where

import           Control.Applicative.Unicode
import           Control.DeepSeq
import           Control.Monad
import           Criterion.Main
import           Data.Hashable
import qualified Data.List                   as List
import           Debug.Trace
import           Data.Typeable
import           GHC.Generics
import           Prelude                     hiding (fromList, length)
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.QuickSpec


type Hash = Int
data Color = Red | Black deriving (Eq, Show, Typeable, Generic, NFData, Ord)

data Set α = Nil | Node Color Hash α (Set α) (Set α)
  deriving (Functor, Foldable, Typeable, Generic, NFData)

instance (Eq α, Ord α) ⇒ Eq (Set α) where
    s₁ == s₂ = List.sort (toList s₁) ≡ List.sort (toList s₂)

instance (Ord α) ⇒ Ord (Set α) where
    s₁ `compare` s₂ = List.sort (toList s₁) `compare` List.sort (toList s₂)

instance (Arbitrary α, Hashable α) ⇒ Arbitrary (Set α) where
    arbitrary = sized genSet
    shrink set = fromList ⦷ shrink (toList set)

instance Show α ⇒ Show (Set α) where
    show Nil = "∅"
    show (Node c _ v l r)  = "Set " ⧺ show c ⧺ " "⧺ show v ⧺
                               "(" ⧺ show l ⧺ ") (" ⧺ show r ⧺ ")"



fromList ∷ Hashable α ⇒ [α] → Set α
fromList = foldr insert Nil


toList ∷ Set α → [α]
toList = foldr (:) []

hashes ∷ Hashable α ⇒ Set α → [Hash]
hashes Nil = []
hashes (Node _ h _ l r) = h : hashes l ⧺ hashes r

member ∷ Hashable α ⇒ α → Set α → Bool
member e = elemʹ (hash e)
    where
      elemʹ h Nil = False
      elemʹ h (Node _ hʹ _ l r)
          | h ≡ hʹ = True
          | h > hʹ = elemʹ h r
          | h < hʹ = elemʹ h l
          | otherwise = impossible "member"


length ∷ Set α → Int
length = List.length ∘ toList


-- delete ∷ Hashable α ⇒ α → Set α → Set α
-- delete (hash → h) Nil = Nil
-- delete (hash → h) (Node c hʹ e l r)
--     | h ≡ hʹ = case c of
--                  Red   → undefined -- Regular BST deletion.
--                  Black → undefined -- absorb the color preserving the invariants
--     | h > hʹ = Node c hʹ e (delete h l) r
--     | h < hʹ = Node c hʹ e l (delete h r)
--     | otherwise = impossible "delete"



insert ∷ Hashable α ⇒ α → Set α → Set α
insert e set =

    let Node _     newHash newV newL newR = insertʹ e set (hash e)
    in  Node Black newHash newV newL newR
        where
          insertʹ e Nil h = Node Red h e Nil Nil
          insertʹ e node@(Node color hʹ eʹ l r) h =
              case compare h hʹ of
                EQ → node
                LT → balance (Node color hʹ eʹ (insertʹ e l h) r)
                GT → balance (Node color hʹ eʹ l (insertʹ e r h))


balance ∷ Set α → Set α
balance (BlackN z zʹ (RedN y yʹ (RedN x xʹ a b)  c                       )  d               ) = RedN y yʹ (BlackN x xʹ a b) (BlackN z zʹ c d)
balance (BlackN z zʹ (RedN x xʹ a                (RedN y yʹ b           c)) d               ) = RedN y yʹ (BlackN x xʹ a b) (BlackN z zʹ c d)
balance (BlackN x xʹ a                           (RedN z zʹ(RedN y yʹ b c)  d              )) = RedN y yʹ (BlackN x xʹ a b) (BlackN z zʹ c d)
balance (BlackN x xʹ a                           (RedN y yʹ b               (RedN z zʹ c d))) = RedN y yʹ (BlackN x xʹ a b) (BlackN z zʹ c d)
balance node                                                                                  = node

pattern BlackN h e l r = Node Black h e l r
pattern RedN   h e l r = Node Red   h e l r

impossible ∷ String → α
impossible label = error ("Impossible: " ⧺ label)


-----------------
-- QuickCheck. --
-----------------


runTests ∷ IO ()
runTests = quickCheckWith settings (conjoin props)
    where
      settings = stdArgs { maxSuccess = 1000 }
      props = [label "noredred"    $ property prop_noRedRed,
               label "evenblack"   $ property prop_evenBlack,
               label "alwaysb"     $ property prop_alwaysBalanced,
               label "depthbounds" $ property prop_depthBounds,
               label "insertid"    $ property prop_insertid,
               label "ordered"     $ property prop_ordered]

      -- Invariant 1.
      prop_noRedRed ∷ Set Int → Bool
      prop_noRedRed  Nil                  = True
      prop_noRedRed (RedN   _ _ RedN{} _) = False
      prop_noRedRed (RedN   _ _ _ RedN{}) = False
      prop_noRedRed (BlackN _ _ l r)      = prop_noRedRed l ∧ prop_noRedRed r
      prop_noRedRed (RedN   _ _ l r)      = prop_noRedRed l ∧ prop_noRedRed r
      prop_noRedRed _                     = impossible "prop_noRedRed"


      -- Invariant 2.
      prop_evenBlack ∷ Set Int → Bool
      prop_evenBlack set = List.length (List.nub (colorCount Black set)) ≡ 1

      prop_alwaysBalanced ∷ Set Int → Bool
      prop_alwaysBalanced set = balance set ≡ set

      prop_depthBounds ∷ Set Int → Bool
      prop_depthBounds set
          | List.length d ≤ 1 = True
          | otherwise         = maximum d ≤ 2 × minimum d
          where d = List.nub (depths set)

      prop_insertid ∷ Int → Set Int → Bool
      prop_insertid x set
          | member x set = insert x set ≡ set
          | otherwise    = let setʹ = insert x set
                           in prop_insertid x setʹ

      prop_ordered ∷ Set Int → Bool
      prop_ordered Nil = True
      prop_ordered (Node _ h _ l@(Node _ h₁ _ _ _) r@(Node _ h₂ _ _ _ )) =
          h > h₁ ∧ h < h₂ ∧ prop_ordered l ∧ prop_ordered r
      prop_ordered (Node _ h _ l@(Node _ h₁ _ _ _) Nil) =
          h > h₁ ∧ prop_ordered l
      prop_ordered (Node _ h _ Nil r@(Node _ h₂ _ _ _ )) =
          h < h₂ ∧ prop_ordered r
      prop_ordered (Node _ _ _ Nil Nil) = True


-- | Count the occurrences of the given color for each path of the tree.
colorCount ∷ Color → Set α → [Int]
colorCount c = count 0
    where
      count acc Nil = [acc]
      count acc (Node cʹ _ _ l r) = count accʹ l ⧺ count accʹ r
          where
            accʹ = if c ≡ cʹ then succ acc else acc


-- | Generate an arbitrary sest of fixed length.
genSet ∷ (Arbitrary α, Hashable α) ⇒ Int → Gen (Set α)
genSet n = do
  as ← replicateM n arbitrary
  return $ fromList as

depths ∷ Set α → [Int]
depths = count 0
    where
      count acc Nil = [acc]
      count acc (Node _ _ _ l r) = count (succ acc) l ⧺ count (succ acc) r


----------------
-- QuickSpec. --
----------------

runSpecs ∷ IO ()
runSpecs = quickSpec [

    prelude (undefined ∷ Int),

    ["set"] `vars` (undefined ∷ Set Int),

    "fromList" `fun1` (fromList ∷ [Int] → Set Int),
    "toList" `fun1` (toList ∷ Set Int → [Int]),

    background $ "sort" `fun1` (List.sort ∷ [Int] → [Int]),

    "Nil" `fun0` (Nil ∷ Set Int),

    background [
        "True" `fun0` True,
        "False" `fun0` False
      ],

    "member" `fun2` (member ∷ Int → Set Int → Bool),

    "insert" `fun2` (insert ∷ Int → Set Int → Set Int),
    "colors" `fun2` ((\c s → List.nub (colorCount c s)) ∷ Color → Set Int → [Int]),

    background ["red" `fun0` Red, "black" `fun0` Black]

  ]


----------------
-- Criterion. --
----------------

benchmarks ∷ IO ()
benchmarks = do
  defaultMain [
--      bgroup "member" $ fmap (\s → bench (show $ length s) (nf checkMember s)) sets,
      bgroup "insert" $ fmap (\s → bench "todo" (nf doInsert    s)) sets
    ]

  return ()
      where
        sets ∷ [Set Int]
        sets = force ∘ traceShow "force" $ fmap (\len → fromList  [1..len]) [2↑x | x ← [10..20]]

--        checkMember = member 1
        doInsert = insert 1000000


main ∷ IO ()
main = benchmarks


