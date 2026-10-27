module Main (main) where

import EvalSpec (evalTests)
import Harness (runGroups)
import InferSpec (inferTests)
import ParserSpec (parserTests)

main :: IO ()
main = runGroups [parserTests, inferTests, evalTests]
