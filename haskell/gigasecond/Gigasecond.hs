{-# LANGUAGE UnicodeSyntax #-}

module Gigasecond (fromDay) where

import           Data.Time.Clock    (UTCTime, addUTCTime)


fromDay ∷ UTCTime → UTCTime
fromDay = addUTCTime 1e9
