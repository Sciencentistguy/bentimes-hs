module Main where

import           Bentimes
import qualified Data.Time     as Time
import           System.Random
main :: IO ()

main = do
    bools <- randomBools
    ints <- randomInts 3
    let relative = getRelative $ head bools
    let interval = getInterval $ head ints
    time <- getTime
    print $ getBentime relative interval time

-- |Generates an infinite list of random booleans
randomBools :: IO [Bool]
randomBools =  randomRs (True,False) <$> getStdGen


-- |Generates an infinite list of random integers, from 0 to a
randomInts :: Int -> IO [Int]
randomInts a =  randomRs (0, a) <$> getStdGen


-- |Returns the current time of day as an IO (hours, minutes)
getTime :: IO Time
getTime = do
    tz <- Time.getCurrentTimeZone
    time <- Time.getCurrentTime
    let zone_diff_seconds = Time.timeZoneMinutes tz * 60
    let seconds = (+ fromIntegral zone_diff_seconds) $ (/10^12) $ fromIntegral $ Time.diffTimeToPicoseconds $ Time.utctDayTime time
    let hours = seconds / 3600
    let mins = floor $ (hours - fromIntegral (floor $ seconds / 3600) ) * 60
    return (floor hours `mod` 12, mins)
