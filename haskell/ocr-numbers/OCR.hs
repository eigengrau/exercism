{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleContexts #-}

module OCR (convert) where

import Prelude.Unicode
import AI.HNN.FF.Network
-- import qualified Numeric.LinearAlgebra as LA
import Data.List.Split
import Prelude.Unicode.SR
import Data.List
import Control.Applicative.Unicode
import GHC.Exts
import Data.Vector.Storable (Vector)
import System.IO.Unsafe
import Control.Monad.Unicode
import Test.QuickCheck.Arbitrary
import Test.QuickCheck hiding (output)
import Control.Monad


convert ∷ String → String
convert input = intercalate "," $ do
  line ← chunkInput input
  let result = convert1 =≪ line
  return result

convert1 ∷ String → String
convert1 input = unsafePerformIO $ do
                   result ← return ∘ ((`runNet` input) =≪) =≪ makeNet
                   return $ case result of
                              Left  err → err
                              Right i   → show i

evalNet ∷ IO (Either String Int)
evalNet = do
  net ← makeNet
  let results = do
            netʹ ← net
            inputsʹ ← inputs
            return $ toList ⦷ fmap (output netʹ tanh) inputsʹ
  return $ fmap maxIndex results
    where inputs = fmap (fmap fst) digitSamples

runNet ∷ Network Double → String → Either String Int
runNet net s = do
  input ←  encodeInput s
  return ∘ maxIndex ∘ toList $ output net tanh input

testNet ∷ Network Double → String → Either String [Double]
testNet net s = do
  input ← encodeInput s
  return ∘ toList $ output net tanh input

makeNet ∷ IO (Either String (Network Double))
makeNet = do
  net ← initialNet
  let netʹ = fmap (train net) digitSamples
  return netʹ
      where
        initialNet = createNetwork 16 [12] 10
        train = trainUntilErrorBelow 6 learnRate  tanh tanh'
        learnRate  = 0.2

-- Encode the digit font as a numeric vector and pair these up with target
-- vectors to be learned.
digitSamples ∷ Either String (Samples Double)
digitSamples = zip ⦷ sequence inputs ⊛ pure targets

    where
      inputs  = fmap encodeInput font
      targets = fmap fromList ∘ take 10 $ iterate rotate (1 : replicate 9 0)
      rotate l = last l : init l

encodeInput ∷ String → Either String (Vector Double)
encodeInput s
    | length s ≢ 16 = Left "?"
    | otherwise     = Right $ fromList (fmap encodeChar s)
    where
      encodeChar ' ' = 0
      encodeChar _   = 1

font ∷ [String]
font = head ∘ chunkInput $ unlines [
        " _     _  _     _  _  _  _  _ ",
        "| |  | _| _||_||_ |_   ||_||_|",
        "|_|  ||_  _|  | _||_|  ||_| _|",
        "                              "
       ]

-- chunkInput ∷ String → [String]
chunkInput input = fmap processRow rows
       where
         processRow = process4 ∘ fmap (chunksOf 3)
         process4 (l1:l2:l3:l4:ls) = [
             unlines [a, b, c, d] | a ← l1
                                  | b ← l2
                                  | c ← l3
                                  | d ← l4
           ]
         rows = chunksOf 4 (lines input)

maxIndex ∷ Ord α ⇒ [α] → Int
maxIndex  xs = snd . minimumBy (flip compare) $ zip xs [0..]

test = unlines
                               [ "    _  _ "
                               , "  | _| _|"
                               , "  ||_  _|"
                               , "         "
                               , "    _  _ "
                               , "|_||_ |_ "
                               , "  | _||_|"
                               , "         "
                               , " _  _  _ "
                               , "  ||_||_|"
                               , "  ||_| _|"
                               , "         " ]
garble = unlines [
          "   "
          , "| |"
          , "| |"
          , "   "
         ]


makeGarble = do
   lines ← replicateM 3 (vectorOf 3 digitElem)
   return $ unlines lines ⧺ "   \n"

        where
          digitElem = elements "|_ "
