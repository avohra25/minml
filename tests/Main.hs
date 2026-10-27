module Main (main) where

import EvalSpec (evalTests)
import Harness (runGroups)
import InferSpec (inferTests)
import ParserSpec (parserTests)
import PropertySpec (propertyTests)

main :: IO ()
main = runGroups [parserTests, inferTests, evalTests, propertyTests]
