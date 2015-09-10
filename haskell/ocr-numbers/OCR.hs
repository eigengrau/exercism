{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE UnicodeSyntax    #-}

module OCR (convert, convertNet) where

import           AI.HNN.FF.Network
import           Control.Monad.Except
import           Control.Monad.Trans.List
import           Control.Monad.Unicode
import           Data.List
import           Data.List.Split
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Vector.Storable     (Vector)
import           GHC.Exts
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           System.IO.Unsafe
import           System.Timeout


-------------------------------------------------
-- A wrapper around the chosen implementation. --
-------------------------------------------------

convert ∷ String → String

#ifdef NEURALNET

convert = unsafePerformIO ∘ convertNet

#else

convert = convertNorm

#endif


-----------------
-- Normal API. --
-----------------

-- | Recognize input.
convertNorm ∷ String → String
convertNorm input = intercalate "," convertedLines

    where
      convertedLines = do
        line ← chunkInput input
        let result = convertNorm1 =≪ line
        return result


-- | Recognize one 16×16 block of input.
convertNorm1 ∷ String → String
convertNorm1 input = maybe "?" show (Map.lookup input fontTable)


-------------------------
-- Neural Network API. --
-------------------------

                                  -- As an alternative, this recognizes the
                                  -- input using a neural network. Obviously,
                                  -- this will also try to make sennse of
                                  -- garbled input, so those tests will fail.


-- | Recognize input using a neural network.
convertNet ∷ String → IO String
convertNet input = fmap (intercalate ",") convertedLines

    where
      convertedLines = runListT $ do
                         line ← toListT $ chunkInput input
                         let result = concat ⦷ mapM convertNet1 line
                         lift result


-- | Recognize one 16×16 block of input.
convertNet1 ∷ String → IO String
convertNet1 input = do

  result ← runExceptT $ runNet network input
  return $ either id show result


-- | Recognizes the input string.
runNet ∷ Monad μ
       ⇒ Network Double         -- ^ The network used for recognition.
       → String                 -- ^ A 16×16 input character.
       → ExceptT String μ Int

runNet net input = do

  encodedInput ← encodeInput input
  let result = output net tanh encodedInput
  return $ maxIndex (toList result)

                                     -- Since the output vectors have one node
                                     -- for each output number, we simply
                                     -- retrieve the number by finding the index
                                     -- of the maximally activated node.


-- | A neural network to recognize character input. Globally initialized for
-- memoization.
network ∷ Network Double
network = unsafePerformIO  $ do

            net ← runExceptT makeNet
            either withError return net

    where
      withError = error ∘ ("Error when instantiating neural network: " ⧺)

{-# NOINLINE network #-}


-- | Generates a neural network and trains it on the font data samples.
makeNet ∷ ExceptT String IO (Network Double)
makeNet = do

  net      ← lift initialNet
  samples  ← digitSamples
  trained  ← lift $ timeout (timeoutSec↑6) (return $! train net samples)
  toExceptT $ maybe (Left "timeout") Right trained

      where
        initialNet = createNetwork 16 [10] 10
        train      = trainUntilErrorBelow 5 learnRate tanh tanh'
        learnRate  = 0.3
        timeoutSec = 10
                                  -- trainUntilError could be trapped in a
                                  -- cycle, so we time it out.


-- | Encode the digit font as a numeric vector and pair these up with target
-- vectors to be learned.
digitSamples ∷ Monad μ ⇒ ExceptT String μ (Samples Double)
digitSamples = do

  inputs ← mapM encodeInput font
  return $ zip inputs targets

    where
      targets = fmap fromList ∘ take 10 $
                  iterate rotate (1 : replicate 9 0)
      rotate l = last l : init l


--------------------------------
-- Shared functions and data. --
--------------------------------

-- | Encode a 16×16 block of input into a feature vector. Throws an error if the
-- block is malformed.
encodeInput ∷ Monad μ
            ⇒ String
            → ExceptT String μ (Vector Double)
encodeInput s
    | length s ≢ 16 = throwError "?"
    | otherwise     = return $ fromList (fmap encodeChar s)
    where
      encodeChar ' ' = 0
      encodeChar  _  = 1


-- | A lookup table which translates glyphs to numbers.
fontTable ∷ Map String Int
fontTable = Map.fromList (zip font [0..9])


-- | An ASCII art font of digits.
font ∷ [String]
font = head ∘ chunkInput $ unlines [
        " _     _  _     _  _  _  _  _ ",
        "| |  | _| _||_||_ |_   ||_||_|",
        "|_|  ||_  _|  | _||_|  ||_| _|",
        "                              "
       ]


-- | Given a stream of ASCII art data, split the data up into visual lines and
-- chunk each line into a 16×16 block.
chunkInput ∷ String → [[String]]
chunkInput input = fmap processRow rows

       where

         -- TODO All the chunking in here is a bit unwieldy to read.

         rows       = chunksOf 4 (lines input)
         processRow = process4 ∘ fmap (chunksOf 3)

         process4 [l1,l2,l3,l4] = [
             unlines [a, b, c, d] | a ← l1
                                  | b ← l2
                                  | c ← l3
                                  | d ← l4
           ]
         process4 _ = error "chunkInput: invalid dimensions"



----------------
-- Utilities. --
----------------

toListT ∷ Monad μ ⇒ [α] → ListT μ α
toListT = ListT ∘ return


toExceptT ∷ Monad μ ⇒ Either α β → ExceptT α μ β
toExceptT = ExceptT ∘ return


-- | Retrieve the index of the maximal element.
maxIndex ∷ Ord α ⇒ [α] → Int
maxIndex xs = snd ∘ maximum $ zip xs [0..]
