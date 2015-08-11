{-# LANGUAGE UnicodeSyntax #-}

module SpaceAge (Planet(..), ageOn) where

import           Prelude.Unicode
import           Prelude.Unicode.SR


data Planet = Earth
            | Mercury
            | Venus
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
            deriving (Eq, Show)


orbitalPeriod ∷ Fractional α ⇒ Planet → α
orbitalPeriod Earth   = 1
orbitalPeriod Mercury = 0.2408467
orbitalPeriod Venus   = 0.61519726
orbitalPeriod Mars    = 1.8808158
orbitalPeriod Jupiter = 11.862615
orbitalPeriod Saturn  = 29.447498
orbitalPeriod Uranus  = 84.016846
orbitalPeriod Neptune = 164.79132


ageOn ∷ Fractional α ⇒ Planet → α → α
ageOn planet ageSeconds = planetYears
  where
    yearLength      = orbitalPeriod planet × secondsPerYear
    planetYears     = ageSeconds ÷ yearLength
    secondsPerDay   = 86400
    secondsPerYear  = 365.25 × secondsPerDay
