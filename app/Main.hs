module Main where

import           Configuration (loadConfiguration)
import           Evaporate (execute)

main :: IO ()
main = loadConfiguration >>= execute
