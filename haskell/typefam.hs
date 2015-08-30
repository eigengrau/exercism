{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE UnicodeSyntax   #-}


import           Data.Singletons
import           Data.Singletons.TH


singletons [d|
  data Foo = X | Y
     deriving Show
  data Empty = Empty deriving Show
 |]


data Box a = Box a


type family a :+: b where
    Sing 'X :+: Sing 'Y = Sing 'Empty


data Test a =
    ∀ x y . (x :+: y) ~ a ⇒ Test x y


a ∷ Sing 'X
a = SX

b ∷ Sing 'Y
b = SY

-- Although this is what GHC infers, and although the types should be
-- equivalent, adding this annotation will cause the program to not
-- typecheck.
c ∷ Test (Sing 'Empty)
--c ∷ Test (Sing 'X :+: Sing 'Y)
c = Test a b


{-
The error reported when using the commented type annotation:

typefam.hs:40:5:
    Couldn't match kind ‘k’ with ‘AnyK’
      ‘k’ is a rigid type variable bound by
          the type signature for c ∷ Test (Sing 'Empty) at typefam.h
s:38:5
    Expected type: Sing 'Empty
      Actual type: Sing 'X :+: Sing 'Y
    Relevant bindings include
      c ∷ Test (Sing 'Empty) (bound at typefam.hs:40:1)
    In the expression: Test a b
    In an equation for ‘c’: c = Test a b
Failed, modules loaded: none.

-}
