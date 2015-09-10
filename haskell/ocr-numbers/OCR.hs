{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE UnicodeSyntax    #-}
{-# LANGUAGE ViewPatterns     #-}

module OCR (convert) where

import           AI.HNN.FF.Network
import           Control.Applicative
import           Control.Applicative.Unicode
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.List
import           Control.Monad.Unicode
import           Data.Either.Combinators
import           Data.List
import           Data.List.Split
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Vector.Storable        (Vector)
import           GHC.Exts
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Statistics.Sample
import           System.IO.Unsafe
import           Test.QuickCheck             hiding (output)


convert ∷ String → String
convert input = intercalate "," $ do
                  line ← chunkInput input
                  let result = convert1 =≪ line
                  return result


convert1 ∷ String → String
convert1 input = maybe "?" show (Map.lookup input fontTable)


net ∷ Network Double
net = fromRight' ∘ unsafePerformIO ∘ runExceptT $ makeNet


convertNet ∷ String → IO String
convertNet input = fmap (intercalate ",") ∘ runListT $ do
                     line ← toListT $ chunkInput input
                     let result = concat ⦷ mapM convertNet1 line
                     lift result


convertNet1 ∷ String → IO String
convertNet1 input = do
  result ← runExceptT (runNet net input)
  return $ case result of
             Left  err     → err
             Right (i,[_,_,skew])
                 | i ≡ 10     → "?" --skew < 0.3 → "?"
                 | otherwise  → show i


comparison ∷ ExceptT String IO [(Int, [Double])]
comparison = do
  net ← makeNet
  normal ← mapM (runNet net) font
  garbled ← mapM (runNet net) garble
  return $ normal ⧺ garbled


evalNet ∷ ExceptT String IO Int
evalNet = do
  net ← makeNet
  (fmap fst → inputs) ← digitSamples
  let results = fmap (toList ∘ output net tanh) inputs
  return $ maxIndex results


runNet ∷ Monad μ
       ⇒ Network Double
       → String
       → ExceptT String μ (Int, [Double])
runNet net s = do
  input ← encodeInput s
  let result = output net tanh input
  return (maxIndex (toList result),
          [kurtosis result, stdDev result, skewness result])


testNet ∷ Monad μ
        ⇒ Network Double
        → String
        → ExceptT String μ [Double]
testNet net s = do
  input ← encodeInput s
  return ∘ toList $ output net tanh input


makeNet ∷ ExceptT String IO (Network Double)
makeNet = do
  net ← lift initialNet
  samples ← liftA2 (⧺) digitSamples garbleSamples
  return $ train net samples
      where
        initialNet = createNetwork 16 [12,12,12] 11
        --train = trainUntilErrorBelow 5 learnRate  tanh tanh'
        train = trainNTimes 10000 learnRate tanh tanh'
        learnRate  = 0.3


-- | Encode the digit font as a numeric vector and pair these up with target
-- vectors to be learned.
digitSamples ∷ Monad μ ⇒ ExceptT String μ (Samples Double)
digitSamples = do
  inputs ← mapM encodeInput font
  return $ zip inputs targets

    where
      targets = fmap (fromList ∘ (⧺[0])) ∘ take 10 $
                  iterate rotate (1 : replicate 9 0)
      rotate l = last l : init l


garbleSamples ∷ Monad μ ⇒ ExceptT String μ (Samples Double)
garbleSamples = do
  inputs ← mapM encodeInput garble
  return $ zip inputs (repeat target)
    where
      target = fromList $ replicate 10 0 ⧺ [1]


encodeInput ∷ Monad μ
            ⇒ String
            → ExceptT String μ (Vector Double)
encodeInput s
    | length s ≢ 16 = throwError "?"
    | otherwise     = return $ fromList (fmap encodeChar s)
    where
      encodeChar ' ' = 0
      encodeChar _   = 1


fontTable ∷ Map String Int
fontTable = Map.fromList (zip font [0..9])


font ∷ [String]
font = head ∘ chunkInput $ unlines [
        " _     _  _     _  _  _  _  _ ",
        "| |  | _| _||_||_ |_   ||_||_|",
        "|_|  ||_  _|  | _||_|  ||_| _|",
        "                              "
       ]


chunkInput ∷ String → [[String]]
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

test ∷ String
test = unlines [
        "    _  _ ",
        "  | _| _|",
        "  ||_  _|",
        "         ",
        "    _  _ ",
        "|_||_ |_ ",
        "  | _||_|",
        "         ",
        " _  _  _ ",
        "  ||_||_|",
        "  ||_| _|",
        "         "
       ]

garble = concat ∘ chunkInput ∘ unlines $ [
          "    _  _ ",
          "     | _ ",
          "  ||_  _|",
          "         ",
          "    _  _ ",
          "| ||  |_ ",
          "  | _|  |",
          "         ",
          " _  _    ",
          "    _|| |",
          "  ||  |_|",
          "         "
         ]

makeGarbleVector ∷ ExceptT String IO (Vector Double)
makeGarbleVector = do
  garble ← lift ∘ generate $ makeGarble `suchThat` (∉ font)
  encodeInput garble


makeGarble ∷ Gen String
makeGarble = do
   lines ← replicateM 3 (vectorOf 3 digitElem)
   return $ unlines lines ⧺ "   \n"

       where
         digitElem = elements "|_ "


toListT ∷ Monad μ ⇒ [α] → ListT μ α
toListT = ListT ∘ return
