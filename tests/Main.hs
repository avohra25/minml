module Main (main) where

import Harness (runGroups)
import ParserSpec (parserTests)

main :: IO ()
main = runGroups [parserTests]
