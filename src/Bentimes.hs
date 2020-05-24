module Bentimes (
    Time,
    getRelative,
    getInterval,
    getBentime,
    ) where

import           Data.Char

getDigitWord :: Int -> String
getDigitWord x
  | n==0 = ""
  | n==1 = "one"
  | n==2 = "two"
  | n==3 = "three"
  | n==4 = "four"
  | n==5 = "five"
  | n==6 = "six"
  | n==7 = "seven"
  | n==8 = "eight"
  | n==9 = "nine"
  where n = x `mod` 10


getTensWord :: Int -> String
getTensWord x
  | n==2 = "twenty"
  | n==3 = "thirty"
  | n==4 = "fourty"
  | n==5 = "fifty"
  | n>5  = error "number over 59 passed to getTensWord"
  | otherwise = ""
  where n = x`div` 10


getNumberWords :: Int -> String
getNumberWords 0 = getNumberWords 12
getNumberWords 10 = "ten"
getNumberWords 11 = "eleven"
getNumberWords 12 = "twelve"
getNumberWords 13 = "thirteen"
getNumberWords 14 = "fourteen"
getNumberWords 15 = "fifteen"
getNumberWords 16 = "sixteen"
getNumberWords 17 = "seventeen"
getNumberWords 18 = "eighteen"
getNumberWords 19 = "nineteen"
getNumberWords x
  | x<10      = getDigitWord x
  | x `mod` 10 == 0 = getTensWord x
  | otherwise = getTensWord x ++ "-" ++ getDigitWord x


data Interval = Ten | Quarter | TwentyFive | Half

instance Show Interval where
    show Ten        = "ten"
    show Quarter    = "quarter"
    show TwentyFive = "twenty-five"
    show Half       = "half"


data Relative = To | Past

instance Show Relative where
    show To   = "to"
    show Past = "past"



getSeparator :: Interval -> Relative -> String
getSeparator Ten To          = "ten to"
getSeparator Quarter To      = "quarter to"
getSeparator TwentyFive To   = "twenty-five to"
getSeparator Half To         = "half to"
getSeparator Ten Past        = "ten past"
getSeparator Quarter Past    = "quarter past"
getSeparator TwentyFive Past = "twenty-five past"
getSeparator Half Past       = "half past"


getInterval :: Int -> Interval
getInterval 0 = Ten
getInterval 1 = Quarter
getInterval 2 = TwentyFive
getInterval 3 = Half
getInterval x = error $ "Invalid number passed to getInterval" ++ show x


getRelative :: Bool -> Relative
getRelative True  = To
getRelative False = Past

type Time = (Int, Int)

getTimeForInterval :: Time -> Relative -> Interval -> Time
getTimeForInterval (hours, mins) Past Ten        = (hours, mins) `timeMinus` 10
getTimeForInterval (hours, mins) Past Quarter    = (hours, mins) `timeMinus` 15
getTimeForInterval (hours, mins) Past TwentyFive = (hours, mins) `timeMinus` 25
getTimeForInterval (hours, mins) Past Half       = (hours, mins) `timeMinus` 30
getTimeForInterval (hours, mins) To Ten          = (hours, mins) `timePlus`  10
getTimeForInterval (hours, mins) To Quarter      = (hours, mins) `timePlus`  15
getTimeForInterval (hours, mins) To TwentyFive   = (hours, mins) `timePlus`  25
getTimeForInterval (hours, mins) To Half         = (hours, mins) `timePlus`  30


showTime :: Time -> String
showTime (hours, 0)    = getNumberWords hours
showTime (hours, 15)   = "quarter past " ++ getNumberWords hours
showTime (hours, 30)   = "half past " ++ getNumberWords hours
showTime (hours, 45)   = "quarter to " ++ getNumberWords (hours+1)
showTime (hours, mins) = getNumberWords mins ++ " past " ++ getNumberWords hours


timeMinus :: Time -> Int -> Time
timeMinus (hours, mins) subtrahend = if mins > subtrahend
                                             then (hours, mins - subtrahend)
                                             else let newhours = if hours - 1 == 0
                                                                 then 12
                                                                 else hours - 1
                                                   in (newhours, mins - subtrahend + 60)


timePlus :: Time -> Int -> Time
timePlus (hours, mins) addend = if mins +addend < 60
                                       then (hours, mins + addend)
                                       else let newhours = if hours + 1 == 13
                                                           then 1
                                                           else hours + 1
                                             in (hours + 1, mins + addend - 60)

getBentime :: Relative -> Interval -> Time -> String
getBentime rel int (hours, mins) = "It is " ++ show int ++ " " ++ show rel ++ " " ++ showTime (newhours, newmins) ++ "."
    where (newhours, newmins) = getTimeForInterval (hours, mins) rel int
