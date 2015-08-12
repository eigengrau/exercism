{-# LANGUAGE UnicodeSyntax #-}

module LinkedList where


data LinkedList α =
    Nil | α :- LinkedList α


nil ∷ LinkedList α
nil = Nil


new ∷ α → LinkedList α → LinkedList α
new = (:-)


isNil ∷ LinkedList α → Bool
isNil Nil = True
isNil _   = False


fromList ∷ [α] → LinkedList α
fromList []     = Nil
fromList (a:as) = a :- fromList as


toList ∷ LinkedList α → [α]
toList Nil       = []
toList (a :- as) = 
