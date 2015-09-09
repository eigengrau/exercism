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
import Statistics.Sample


convert ∷ String → String
convert input = intercalate "," $ do
  line ← chunkInput input
  let result = convert1 =≪ line
  return result

convert1 ∷ String → String
convert1 input = unsafePerformIO $ do
                   result ←
                       return ∘ ((`runNet` input) =≪) =≪ makeNet
                   return $ case result of
                              Left  err     → err
                              Right (i,[_,_,skew]) → if skew < 0.3
                                                     then "?"
                                                     else show i

comparison = do
  net ← makeNet
  let normal = fmap (fmap snd) (mapM (\c → net ≫= (`runNet` c)) font)
      garbled = fmap (fmap snd) (mapM (\c → net ≫= (`runNet` c)) garble)
  return ((,) ⦷ normal ⊛ garbled)


evalNet ∷ IO (Either String Int)
evalNet = do
  net ← makeNet
  let results = do
            netʹ ← net
            inputsʹ ← inputs
            return $ toList ⦷ fmap (output netʹ tanh) inputsʹ
  return $ fmap maxIndex results
    where inputs = fmap (fmap fst) digitSamples

runNet ∷ Network Double → String → Either String (Int, [Double])
runNet net s = do
  input ← encodeInput s
  let result = output net tanh input
  return (maxIndex (toList result),
          [kurtosis result, stdDev result, skewness result])

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
        initialNet = createNetwork 16 [12,12,12] 10
        train = trainUntilErrorBelow 5 learnRate  tanh tanh'
        --train = trainNTimes 10000 learnRate  tanh tanh'
        learnRate  = 0.3

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
garble = [unlines [
           "   "
          , "| |"
          , "| |"
          , "   "
          ],
          unlines [
           " _ "
          ,"| |"
          ," _|"
          ,"   "
          ],
          unlines [
           "   "
          ,"| |"
          ,"|_|"
          ,"   "
          ],
          unlines [
           "   "
          ,"  |"
          ,"  |"
          ,"   "
          ]
         ]

makeGarbleVector ∷ IO (Either String (Vector Double))
makeGarbleVector = do
  garble ← generate $ makeGarble `suchThat` (\x → x ∉ font)
  return $ encodeInput garble

makeGarble = do
   lines ← replicateM 3 (vectorOf 3 digitElem)
   return $ unlines lines ⧺ "   \n"

        where
          digitElem = elements "|_ "

-- Garbled
    -- [[- 1.5441037975666287, 0.29330325405980856, - 0.2260091809091274,
    --   0.11723915215092957],
    --  [- 1.4732096692291283, 0.3245441592306161, 4.950756998173012e-2,
    --   4.7804026233086785e-2],
    --  [- 1.3122005521609603, 0.28991121525613256, 0.44263129640128857,
    --   0.17089298584051682],
    --  [0.1285626045970818, 0.34271021441644595, - 1.0188292255815243,
    --   4.937116638166379e-2],
    --  [- 1.2974311719339153, 0.2630574271281181, 0.28588649533692656,
    --   0.11625291941181826],
    --  [- 1.3763423449496064, 0.28204540188843374, 2.6822776105237964e-2,
    --   0.11439089510965957],
    --  [- 1.0218224710322605, 0.287737622429675, - 0.4171014265145352,
    --   6.447778721893596e-2],
    --  [- 1.0494324095001377, 0.3100839049851633, - 0.13064852568674057,
    --   7.760472195892168e-2],
    --  [- 0.901716583951651, 0.33550620418363863, - 0.3328680973460485,
    --   5.975566043899061e-2],
    --  [- 0.36261148053709613, 0.3095065930143726, - 0.7406564137487083,
    --   8.661348031570903e-2]]


-- Ungarbled 
    -- [[0.9086649225282404, 0.2506753790746807, 1.2701933631112652,
    --   7.898865722165958e-2],
    --  [1.6537449428539146, 0.24764889448608832, 1.4875067952479646,
    --   8.81243266806404e-2],
    --  [3.982172609718612, 0.25931462924658555, 2.3077314293634643,
    --   9.732240674539154e-2],
    --  [2.9951925801246944, 0.2668305414659457, 2.000031631029371,
    --   7.395352262079175e-2],
    --  [2.6467907907900257, 0.2412244361629522, 1.6603937664390742,
    --   7.37352615667335e-2],
    --  [0.17196427685012505, 0.25189782269293365, 0.6461747750249938,
    --   7.363447093796949e-2],
    --  [- 0.3944685229216942, 0.24902611755424897, 0.6839705863044155,
    --   9.982194718475182e-2],
    --  [3.9432702885830127, 0.2884286637614056, 2.2769028628822943,
    --   9.41361011126425e-2],
    --  [- 0.19622540655365306, 0.22133785664290948, 0.19556183875173613,
    --   0.1113090419757485],
    --  [2.8186089787045203, 0.24844319612347443, 1.8458664012483963,
    --   6.282678607447303e-2]]
