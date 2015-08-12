{-# LANGUAGE UnicodeSyntax #-}

module Robot (robotName, mkRobot, resetName) where

import Prelude.Unicode
import System.Random
import Data.IntMap


data Robot = Robot { robotName ∷ String }
    deriving (Eq, Show)


data RobotFactory = RobotFactory (Map Robot)


instance Monad RobotFactory where
    return = RobotFactory []
    factory >>= fun = undefined

mkRobot ∷ RobotFactory Robot
mkRobot = do undefined


resetName ∷ RobotFactory Robot → Robot
resetName = _
