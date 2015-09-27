{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module CustomSet where

import           Control.Applicative.Unicode
import           Control.Applicative
import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Unicode
import           Control.Monad.Writer
import           Criterion.Main
import           Data.Hashable
import qualified Data.List                   as List
import qualified Data.Tree                   as Tree
import           Data.Tree.Pretty
import           Data.Typeable
import           Debug.Trace
import           GHC.Generics
import           GHC.GHCi.PPrint
import           Prelude                     hiding (fromList, length)
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.QuickSpec
import GHC.List (foldl1')


type Hash = Int
data Color = Red | Black | DBlack deriving (Eq, Show, Typeable, Generic, NFData, Ord)

data Set α = Nil | Node {
      nodeColor ∷ Color,
      nodeValue ∷ α,
      nodeLeft  ∷ Set α,
      nodeRight ∷ Set α
    } deriving (Functor, Foldable, Typeable, Generic, NFData)

instance (Eq α, Ord α) ⇒ Eq (Set α) where
    s₁ == s₂ = List.sort (toList s₁) ≡ List.sort (toList s₂)

instance (Ord α) ⇒ Ord (Set α) where
    s₁ `compare` s₂ = List.sort (toList s₁) `compare` List.sort (toList s₂)

instance (Arbitrary α, Ord α, Show α) ⇒ Arbitrary (Set α) where
    arbitrary = sized genSet
    shrink set = fromList ⦷ shrink (toList set)

instance (Ord α, Show α) ⇒ Show (Set α) where
    show Nil = "∅"

    show (Node c v l r) = "Set " ⧺ show c ⧺ " "⧺ show v ⧺
                            "(" ⧺ show l ⧺ ") (" ⧺ show r ⧺ ")"


toTree ∷ Ord α ⇒ Set α → Tree.Tree α
toTree Nil = undefined
toTree n@(Node _ v l r ) = Tree.Node v (left ⧺ right)
    where
      left  = if l ≡ Nil then [] else [toTree l]
      right = if r ≡ Nil then [] else [toTree r]


pretty ∷ (Show α, Ord α) ⇒ Set α → IO ()
pretty = putStrLn ∘ drawVerticalTree ∘ fmap show ∘ toTree


fromList ∷ Ord α ⇒ [α] → Set α
fromList = foldr insert Nil


toList ∷ Set α → [α]
toList = foldr (:) []


member ∷ Ord α ⇒ α → Set α → Bool
member e Nil = False
member e (Node _ eʹ l r)
    | e ≡ eʹ = True
    | e > eʹ = member e r
    | e < eʹ = member e l
    | otherwise = impossible "member"


length ∷ Set α → Int
length = List.length ∘ toList


delete ∷ Ord α ⇒ α → Set α → Set α
delete v Nil = Nil
delete v (Node c vʹ l r)
    | v ≡ vʹ = case c of
                 Red   → undefined -- Regular BST deletion.
                 Black → undefined -- absorb the color preserving the invariants
    | v < vʹ = Node c vʹ (delete v l) r
    | v > vʹ = Node c vʹ l (delete v r)
    | otherwise = impossible "delete"

rbDelete ∷ (Show α, Ord α) ⇒ α → Set α → Set α
rbDelete _ Nil                = Nil
rbDelete _ (Node _ _ Nil Nil) = Nil
rbDelete v (Node c v₀ n@(Node c₁ v₁ l₁ r₁) Nil)
    | v ≡ v₀ = n
    | otherwise = Node c v₀ (rbDelete v n) Nil
rbDelete v (Node c v₀ Nil n@(Node c₁ v₁ l₁ r₁))
    | v ≡ v₀ = n
    | otherwise = Node c v₀ Nil (rbDelete v n)
rbDelete v n@(Node c v₀ l@(Node c₁ v₁ l₁ r₁) r@(Node c₂ v₂ l₂ r₂))
    | v > v₀ = absorb $ Node c v₀ l (rbDelete v r)
    | v < v₀ = absorb $ Node c v₀(rbDelete v l) r
    | v ≡ v₀ = let Node c₃ v₃ _ _  = rightMost l
               in case c of
                    Red   → Node c₃ v₃ (rbDelete v₃ l) r
                    Black → Node DBlack v₃ (rbDelete v₃ l) r
                    _     → error "Imaginary color during rbDelete"
    | otherwise = impossible "rbdelete"

absorb ∷ (Show α, Ord α) ⇒ Set α → Set α
absorb Nil = Nil
absorb b@(RedN vb
         a@DBlackN{}
         d@(BlackN _
          c@BlackN{}
          e@BlackN{})) = BlackN vb a (paint Red d)
absorb b@(RedN vb
         d@(BlackN _
          c@BlackN{}
          e@BlackN{})
         a@DBlackN{}) = BlackN vb (paint Red d) a
absorb b@(BlackN vb
         a@DBlackN{}
         d@(BlackN _
          c@BlackN{}
          e@BlackN{})) = DBlackN vb a (paint Red d)
absorb b@(BlackN vb
         d@(BlackN _
          c@BlackN{}
          e@BlackN{})
         a@DBlackN{}) = DBlackN vb (paint Red d) a
absorb b@(RedN vb
         a@DBlackN{}
         d@(BlackN vd
          c@BlackN{}
          e@RedN{})) = RedN vd (paint Black e) (BlackN vb a c)
absorb b@(RedN vb
         d@(BlackN vd
          c@BlackN{}
          e@RedN{})
         a@DBlackN{}) = RedN vd (paint Black e) (BlackN vb a c)
absorb (BlackN _
         BlackN{}
         (BlackN _
           RedN{}
           DBlackN{})) = undefined
absorb n@DBlackN{} = n
absorb n
    | sum (colorCount DBlack n) ≢ 0 = error ("Not absorbed:" ⧺ show n)
    | otherwise = n

-- TODO Nil counts as black

paint ∷ Color → Set α → Set α
paint Black Nil = Nil
paint _ Nil = undefined
paint c (Node _ v l r) = Node c v l r


pattern DBlackN v l r = Node DBlack v l r

zoom v Nil = Nil
zoom v n@(Node _ vʹ l r)
    | v < vʹ = zoom v l
    | v > vʹ = zoom v r
    | v ≡ vʹ = n
    | otherwise = impossible "zoom"

bstDelete ∷ Ord α ⇒ α → Set α → Set α
bstDelete _ (Node _ _ Nil Nil) = Nil
bstDelete v (Node c v₀ n@(Node c₁ v₁ l₁ r₁) Nil)
    | v ≡ v₀ = n
    | otherwise = Node c v₀ (bstDelete v n) Nil
bstDelete v (Node c v₀ Nil n@(Node c₁ v₁ l₁ r₁))
    | v ≡ v₀ = n
    | otherwise = Node c v₀ Nil (bstDelete v n)
bstDelete v n@(Node c v₀ l@(Node c₁ v₁ l₁ r₁) r@(Node c₂ v₂ l₂ r₂))
    | v > v₀ = Node c v₀ l (bstDelete v r)
    | v < v₀ = Node c v₀(bstDelete v l) r
    | v ≡ v₀ = let Node c₃ v₃ _ _  = rightMost l
               in  Node c₃ v₃ (bstDelete v₃ l) r


isPreterminal ∷ Set α → Bool
isPreterminal (Node _ _ Nil Nil) = True
isPreterminal _                  = False

isSingleMother ∷ Set α → Bool
isSingleMother Nil = undefined
isSingleMother (Node _ _ Nil Node{}) = True
isSingleMother (Node _ _ Node{} Nil) = True
isSingleMother (Node _ _ Node{} Node{}) = False
isSingleMother (Node _ _ Nil    Nil)    = False

rightMost ∷ Set α → Set α
rightMost Nil = Nil
rightMost n@(Node _ _ _ Nil) = n
rightMost n@(Node _ _ _ r  ) = rightMost r



insert ∷ Ord α ⇒ α → Set α → Set α
insert e set = blacken (insertʹ e set)

    where
      insertʹ e Nil = Node Red e Nil Nil
      insertʹ e node@(Node color eʹ l r) =
          case compare e eʹ of
            EQ → node
            LT → balance (Node color eʹ (insertʹ e l) r)
            GT → balance (Node color eʹ l (insertʹ e r))

      blacken (Node _ v l r) = Node Black v l r
      blacken Nil            = Nil


balance ∷ Set α → Set α
balance (BlackN z (RedN y (RedN x a b)  c           ) d                                   ) = RedN y (BlackN x a b) (BlackN z c d)
balance (BlackN z (RedN x a             (RedN y b c)) d                                   ) = RedN y (BlackN x a b) (BlackN z c d)
balance (BlackN x a                                  (RedN z (RedN y  b c)  d            )) = RedN y (BlackN x a b) (BlackN z c d)
balance (BlackN x a                                  (RedN y b               (RedN z c d))) = RedN y (BlackN x a b) (BlackN z c d)
balance node                                                                                = node

pattern BlackN e l r = Node Black e l r
pattern RedN   e l r = Node Red   e l r

impossible ∷ String → α
impossible label = error ("Impossible: " ⧺ label)


-----------------
-- QuickCheck. --
-----------------


runTests ∷ IO ()
runTests =
  forM_ props (\(l,prop) → putStr (l ⧺ ": ") ≫ quickCheckWith settings prop)
    where
      settings = stdArgs { maxSuccess = 300 }
      props = [("noredred"    , property prop_noRedRed),
               ("evenblack"   , property prop_evenBlack),
               ("alwaysb"     , property prop_alwaysBalanced),
               ("depthbounds" , property prop_depthBounds),
               ("insertid"    , property prop_insertid),
               ("ordered"     , property prop_ordered),
               ("bstdel"      , property prop_bstdel),
               ("rbdel"       , property prop_rbdel)]

      -- Invariant 1.
      prop_noRedRed ∷ Set Int → Bool
      prop_noRedRed  Nil                  = True
      prop_noRedRed (RedN   _ RedN{} _)   = False
      prop_noRedRed (RedN   _ _ RedN{})   = False
      prop_noRedRed (BlackN _ l r)        = prop_noRedRed l ∧ prop_noRedRed r
      prop_noRedRed (RedN   _ l r)        = prop_noRedRed l ∧ prop_noRedRed r
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
      prop_ordered (Node _ h l@(Node _ h₁ _ _) r@(Node _ h₂ _ _ )) =
          h > h₁ ∧ h < h₂ ∧ prop_ordered l ∧ prop_ordered r
      prop_ordered (Node _ h l@(Node _ h₁ _ _) Nil) =
          h > h₁ ∧ prop_ordered l
      prop_ordered (Node _ h Nil r@(Node _ h₂ _ _ )) =
          h < h₂ ∧ prop_ordered r
      prop_ordered (Node _ _ Nil Nil) = True

      prop_bstdel ∷ Set Int → Int → Bool
      prop_bstdel set x
          | member x set = List.sort (toList (bstDelete x set)) ≡ List.sort (List.delete x (toList set))
          | otherwise    = bstDelete x (insert x set) ≡ set

      prop_rbdel ∷ Set Int → Int → Bool
      prop_rbdel set x
          | member x set =
              let setʹ = rbDelete x set
              in prop_noRedRed setʹ ∧ prop_evenBlack setʹ ∧ List.sort (toList setʹ) ≡ List.sort (List.delete x (toList set))
          | otherwise  =
              let setʹ = rbDelete x (insert x set)
              in prop_noRedRed setʹ ∧ prop_evenBlack setʹ ∧ setʹ ≡ set

-- | Count the occurrences of the given color for each path of the tree.
colorCount ∷ Color → Set α → [Int]
colorCount c = count 0
    where
      count acc Nil = [acc]
      count acc (Node cʹ _ l r) = count accʹ l ⧺ count accʹ r
          where
            accʹ = if c ≡ cʹ then succ acc else acc


-- | Generate an arbitrary sest of fixed length.
genSet ∷ (Arbitrary α, Ord α, Show α) ⇒ Int → Gen (Set α)
genSet n = do
  let as = fromList []
  insertions ← elements [1..10]
  deletions  ← elements [1..10]
  ops ← shuffle (replicate insertions doIns ⧺ replicate deletions doDel)
  set ← foldl1' (>=>) ops as
  case compare (length set) n of
    EQ → return set
    LT → iterateUntilM ((≥n) ∘ length) doIns set
    GT → iterateUntilM ((≤n) ∘ length) doDel set


    where
      doDel ∷ (Ord α, Show α) ⇒ Set α → Gen (Set α)
      doDel s
          | List.null (toList s) = return s
          | otherwise = do
               x ← elements (toList s)
               return (rbDelete x s)

      doIns ∷ (Ord α, Arbitrary α, Show α) ⇒ Set α → Gen (Set α)
      doIns s = do
             x ← resize (10↑5) arbitrary `suchThat` (not ∘ (`member` s))
             return (insert x s)


depths ∷ Set α → [Int]
depths = count 0
    where
      count acc Nil = [acc]
      count acc (Node _ _ l r) = count (succ acc) l ⧺ count (succ acc) r


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
                                     -- Weirdly, evaluating doInsert s to
                                     -- normal-form will yield linear runtimes
                                     -- for insertion, even when the profiler
                                     -- shows that insert is only called
                                     -- logarithmically often. The difference
                                     -- lies in checkMember only returning a
                                     -- Bool, so NF and WHNF are the same in
                                     -- this instance.
                                     --
                                     -- I’m not yet sure though why whnf will
                                     -- force the whole tree all over again,
                                     -- since I’ve forced all the input sets
                                     -- already.

  defaultMain [
      bgroup "member" $ fmap (\s → bench (show $ length s) (whnf checkMember s)) sets,
      bgroup "insert" $ fmap (\s → bench (show $ length s) (whnf doInsert    s)) sets
    ]

  return ()

    where
      sets ∷ [Set Int]
      sets = force ∘ traceShow "force" $ fmap (\len → fromList  [1..len]) [2↑x | x ← [10..20]]

      checkMember = member 1
      doInsert    = insert 1000000



main ∷ IO ()
main = benchmarks


