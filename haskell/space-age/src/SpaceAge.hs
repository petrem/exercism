module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune deriving (Eq, Ord, Show)


relativeOrbitalPeriod :: Planet -> Double
relativeOrbitalPeriod planet = case planet of
                                 Earth -> 1
                                 Mercury -> 0.2408467
                                 Venus -> 0.61519726
                                 Mars -> 1.8808158
                                 Jupiter -> 11.862615
                                 Saturn -> 29.447498
                                 Uranus -> 84.016846
                                 Neptune -> 164.79132

earthYear :: Double
earthYear = 31557600


-- An unbiased, "human-like" round, as most of us would expect round to work
-- e.g. roundHalfAwayFromZero 1.525 2 == 1.53
-- but  roundTo 2 1.525 == 1.52
--        where roundTo n = (/ 10 ^ n) . fromIntegral . round . (* 10 ^ n)

roundHalfAwayFromZero :: Double -> Int -> Double
roundHalfAwayFromZero x = (/) <$> fromInteger . floor . (+ 0.5) . (* x) . (10 ^^) <*> (10 ^^)

ageOn :: Planet -> Double -> Double
ageOn planet = flip roundHalfAwayFromZero 2 . (/ relativeOrbitalPeriod planet) . (/ earthYear)
