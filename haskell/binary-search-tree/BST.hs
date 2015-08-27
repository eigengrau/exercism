{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UnicodeSyntax #-}

module BST (
    bstLeft, bstRight,
    fromList, toList,
    bstValue,
    singleton,
    insert
  ) where

import           Data.Foldable      hiding (toList)
import           Prelude.Unicode
import           Prelude.Unicode.SR


----------
-- API. --
----------

bstLeft, bstRight ∷ Tree α
                  → Maybe (Tree α)
bstLeft  (Node _ l _) = Just l
bstLeft  Nil          = Nothing
bstRight (Node _ _ r) = Just r
bstRight Nil          = Nothing


insert ∷ Ord α ⇒ α → Tree α → Tree α
insert a Nil = Node a Nil Nil
insert a (Node n l r)
    | a ≤ n     = Node n (insert a l) r
    | otherwise = Node n l (insert a r)


singleton ∷ α → Tree α
singleton a = Node a Nil Nil


fromList ∷ Ord α ⇒ [α] → Tree α
fromList = foldl' (flip insert) Nil


toList ∷ Tree α → [α]
toList Nil          = []
toList (Node a l r) = toList l ⧺ a : toList r


bstValue ∷ Tree α → α
bstValue (Node a _ _) = a
bstValue Nil          = error "bstValue: empty tree."


pretty ∷ Show α ⇒ Tree α → String                               -- Just for fun.
pretty = unlines ∘ foldMap return ∘ indent ∘ fmap show

ppretty ∷ Show α ⇒ Tree α → IO ()
ppretty = putStrLn ∘ pretty


------------
-- Types. --
------------

data Tree α = Node α (Tree α) (Tree α)
            | Nil
            deriving (Functor, Show)



instance Foldable Tree where                          -- Useful for
                                                      -- pretty-printing.
    foldMap _  Nil         = mempty
    foldMap f (Node v l r) = f v ⊕ foldMap f l ⊕ foldMap f r


----------------
-- Utilities. --
----------------

indent ∷ Tree String → Tree String
indent = indentʹ 0

    where
      indentʹ _ Nil = Nil
      indentʹ d (Node a l r) =

          Node (replicate d ' ' ⧺ a)
            (succ d `indentʹ` l)
            (succ d `indentʹ` r)
