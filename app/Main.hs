module Main where

import           Bentimes
import qualified Data.Time as Time

main :: IO()
main = do
    t <- Time.getCurrentTime
    lt <- Time.utcToLocalZonedTime t
    print lt
