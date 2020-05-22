module Bentimes (
    getNumberWords,
    capitaliseWord,
    getSeparator,
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
  | n>5  = error "number over 59 sent to getTensWord"
  | otherwise = ""
  where n = x`div` 10


getNumberWords :: Int -> String
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
getNumberWords x  = getTensWord x ++ "-" ++ getDigitWord x


capitaliseWord :: String -> String
capitaliseWord (x:xs) = toUpper x : xs


data Interval = Quarter | Ten | Half | TwentyFive

data Relative = To | Past


getSeparator :: Interval -> Relative -> String
getSeparator Quarter To      = "quarter to"
getSeparator Ten To          = "ten to"
--getSeparator Half To = "half to"
getSeparator TwentyFive To   = "twenty-five to"
getSeparator Quarter Past    = "quarter past"
getSeparator Ten Past        = "teb past"
getSeparator Half Past       = "half past"
getSeparator TwentyFive Past = "twenty-five past"
