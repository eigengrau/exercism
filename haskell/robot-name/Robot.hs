{-# LANGUAGE UnicodeSyntax #-}

module Robot (robotName, mkRobot, resetName) where

import           Control.Conditional  (ifM)
import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.State
import           Data.Global          (declareIORef)
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IntMap
import           Data.IORef
import           Data.Maybe
import           Prelude.Unicode


------------
-- Types. --
------------

-- Robots store unique identifiers which never change, even when the
-- name is reset. A RobotFactory stores the mapping from IDs to names.
type RobotFactory = IntMap RobotName
type RobotID = Int
data Robot = Robot { robotID ∷ RobotID }
    deriving (Eq, Show, Ord)


newtype RobotName = RobotName { getRobotName ∷ String }
    deriving (Eq, Ord)

instance Random RobotName where
  randomR (_, _) = random

  random = runRand $ do
    prefix ← replicateM 2 (uniform ['A'..'Z'])
    number ← replicateM 3 (uniform ['0'..'9'])
    return $ RobotName (prefix ⧺ number)

instance Show RobotName where
  show = getRobotName


-----------------------------------
-- Internal API which avoids IO. --
-----------------------------------

mkRobotʹ ∷ StateT RobotFactory (Rand StdGen) Robot
mkRobotʹ = do

  newName ← newUniqueName
  newID ← nextID
  let newRobot = Robot newID
  modify (IntMap.insert newID newName)
  return newRobot


resetNameʹ ∷ Robot → StateT RobotFactory (Rand StdGen) ()
resetNameʹ robot = do
  newName ← newUniqueName
  modify (IntMap.insert (robotID robot) newName)


nextID ∷ Monad μ ⇒ StateT RobotFactory μ RobotID
nextID = do
  factory ← get
  let allIDs = IntMap.keys factory
  if null allIDs
  then return 1
  else return $ succ (maximum allIDs)


newUniqueName ∷ StateT RobotFactory (Rand StdGen) RobotName
newUniqueName = do
  newName ← lift $ liftRand random
  ifM (isNameUnique newName) (return newName) newUniqueName

    where
      isNameUnique ∷ Monad μ ⇒ RobotName → StateT RobotFactory μ Bool
      isNameUnique name = do
        factory ← get
        let allNames = IntMap.elems factory
        return $ name ∉ allNames


-------------------------------------------------------------------------
-- Apparently HUnit testcases work only in IO. To accomodate for the   --
-- state based design, this uses a global IORef to keep global without --
-- requiring the test cases to be changed.                             --
-------------------------------------------------------------------------

mkRobot ∷ IO Robot
mkRobot = inGlobalState $ do
  (gen, factory) ← get
  let ((newRobot, factoryʹ), genʹ) = runStateT mkRobotʹ factory `runRand` gen
  put (genʹ, factoryʹ)
  return newRobot


robotName ∷ Robot → IO String
robotName robot = inGlobalState $ do
  (_, factory) ← get
  let name  = getRobotName <$> IntMap.lookup (robotID robot) factory
      nameʹ = fromMaybe (error "Robot not found") name
  return nameʹ


resetName ∷ Robot → IO ()
resetName robot = inGlobalState $ do
  (gen, factory) ← get
  let ((_, factoryʹ), genʹ) = runRand (runStateT (resetNameʹ robot) factory) gen
  put (genʹ, factoryʹ)
  return ()


globalFactory ∷ IORef RobotFactory
globalFactory = declareIORef "global-robot-factory" IntMap.empty


inGlobalState ∷ State (StdGen, RobotFactory) α → IO α
inGlobalState fun = do
  gen ← getStdGen
  factory ← readIORef globalFactory
  let (result, (genʹ, factoryʹ)) = runState fun (gen, factory)
  writeIORef globalFactory factoryʹ
  setStdGen genʹ
  return result
