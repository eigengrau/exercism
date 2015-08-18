{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}

module Robot (
    Bearing(..),
    Robot,
    mkRobot,
    coordinates,
    simulate,
    bearing,
    turnRight,
    turnLeft
  ) where

import           Data.Char
import           Data.List       as List
import           Prelude.Unicode


------------
-- Types. --
------------

data Robot = Robot {
      getBearing  ∷ Bearing,
      getPosition ∷ Position
    } deriving (Show, Eq)


type Position = (Int, Int)

type Instruction = Char
type Program = [Instruction]

data Bearing = North | East | South | West
               deriving (Show, Eq)

instance Enum Bearing where

    succ North = East
    succ East  = South
    succ South = West
    succ West  = North

    pred = succ ∘ succ ∘ succ

    fromEnum North = 0
    fromEnum East  = 1
    fromEnum South = 2
    fromEnum West  = 3

    toEnum 0 = North
    toEnum 1 = East
    toEnum 2 = South
    toEnum 3 = West
    toEnum _ = undefined


----------
-- API. --
----------

mkRobot ∷ Bearing → Position → Robot
mkRobot = Robot


coordinates ∷ Robot → Position
coordinates = getPosition


bearing ∷ Robot → Bearing
bearing = getBearing


simulate ∷ Robot → Program → Robot
simulate robot (normalizeProgram → progn) =
    List.foldl' decode robot progn

    where
      decode Robot{..} 'l' = Robot (turnLeft  getBearing) getPosition
      decode Robot{..} 'r' = Robot (turnRight getBearing) getPosition
      decode Robot{..} 'a' = Robot getBearing (advance getBearing getPosition)
      decode _ instruction =
          error ("simulate: invalid instruction" ⧺ show instruction)

      advance West  (x,y) = (pred x, y)
      advance East  (x,y) = (succ x, y)
      advance North (x,y) = (x, succ y)
      advance South (x,y) = (x, pred y)


turnRight, turnLeft ∷ Bearing → Bearing
turnRight = succ
turnLeft  = pred


----------------
-- Utilities. --
----------------

normalizeProgram ∷ Program → Program
normalizeProgram = fmap toLower
