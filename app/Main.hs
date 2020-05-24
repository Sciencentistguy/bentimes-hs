module Main where

import           Bentimes
import qualified Data.Time     as Time
import           System.Random
main :: IO ()

main = do
    print "temp"


-- |Generates an infinite list of random booleans
randomBools :: IO [Bool]
randomBools =  randomRs (True,False) <$> getStdGen


-- |Generates an infinite list of random integers, from 0 to a
randomInts :: Int -> IO [Int]
randomInts a =  randomRs (0, a) <$> getStdGen


-- |Returns the current time of day as an IO (hours, minutes)
getTimeInts :: Integral a => IO (a, a)
getTimeInts = do
    tz <- Time.getCurrentTimeZone
    time <- Time.getCurrentTime
    let zone_diff_seconds = Time.timeZoneMinutes tz * 60
    let seconds = (+ fromIntegral zone_diff_seconds) $ (/10^12) $ fromIntegral $ Time.diffTimeToPicoseconds $ Time.utctDayTime time
    let hours = seconds / 3600
    let mins = floor $ (hours - fromIntegral (floor $ seconds / 3600) ) * 60
    return (floor hours `mod` 12, mins)


-- TODO:
-- a function that takes true or false and returns an interval to or past
-- a function that takes a number 0,1,2,3 and returns (ten quarter twentyfive half)
-- a function that takes a (to/past) and a (ten quarter twentyfive half) and returns a string
