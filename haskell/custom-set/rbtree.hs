{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE UnicodeSyntax      #-}

module RBTest where

import           Control.Applicative.Unicode
import           Control.DeepSeq
import           Control.Monad
import           Criterion.Main
import           Data.Hashable
import qualified Data.List                   as List
import           Data.Typeable
import           Debug.Trace
import           GHC.Generics
import           OrPatterns
import           Prelude                     hiding (fromList, length)
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.QuickSpec
import Data.Tree.RBTree

----------------
-- Criterion. --
----------------

deriving instance Generic (RBTree α)
deriving instance Typeable RBTree
deriving instance NFData Color
deriving instance Generic Color
deriving instance NFData α ⇒ NFData (RBTree α)

fromList = foldr (flip insertOrd) emptyRB

forceTree ∷ NFData α ⇒ RBTree α → RBTree α
forceTree Leaf = Leaf
forceTree t@(Node color v l r) = color `deepseq` v `deepseq` force l `deepseq` force r `deepseq` t

benchmarks ∷ IO ()
benchmarks = do
  defaultMain [
--      bgroup "member" $ fmap (\s → bench (show $ length s) (nf checkMember s)) sets,
      bgroup "insert" $ fmap (\s → bench "todo" (nf doInsert s)) sets
    ]

  return ()
      where
        sets ∷ [RBTree Int]
        sets = force ∘ traceShow "force" $ fmap (\len → fromList  [1..len]) [2↑x | x ← [10..20]]

--        checkMember = member 1
        doInsert = (flip insertOrd) 1000000

main = benchmarks
