-- | A tiny dependency-free test harness.
module Harness
  ( Test
  , test
  , group
  , assertEq
  , assertBool
  , assertLeft
  , runGroups
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM)
import System.Exit (exitFailure)

data Test = Test String (Either String ())

type Group = (String, [Test])

test :: String -> Either String () -> Test
test = Test

group :: String -> [Test] -> Group
group = (,)

assertEq :: (Eq a, Show a) => a -> a -> Either String ()
assertEq expected actual
  | expected == actual = Right ()
  | otherwise = Left ("expected: " ++ show expected ++ "\n       got: " ++ show actual)

assertBool :: String -> Bool -> Either String ()
assertBool _ True = Right ()
assertBool msg False = Left msg

-- | Passes when the value is a 'Left' whose message contains the needle.
assertLeft :: Show b => String -> Either String b -> Either String ()
assertLeft needle r = case r of
  Left msg | needle `isInfixOf'` msg -> Right ()
           | otherwise -> Left ("error " ++ show msg ++ " does not mention " ++ show needle)
  Right v -> Left ("expected an error mentioning " ++ show needle ++ ", got " ++ show v)
  where
    isInfixOf' n h = any (startsWith n) (tails' h)
    startsWith n h = take (length n) h == n
    tails' [] = [[]]
    tails' s@(_ : xs) = s : tails' xs

runGroups :: [Group] -> IO ()
runGroups groups = do
  results <- forM groups $ \(name, tests) -> do
    rs <- forM tests $ \(Test tname r) -> do
      out <- try (evaluate (forceResult r)) :: IO (Either SomeException (Either String ()))
      case out of
        Right (Right ()) -> pure True
        Right (Left msg) -> report tname msg >> pure False
        Left ex -> report tname ("exception: " ++ show ex) >> pure False
    let passed = length (filter id rs)
    putStrLn (name ++ ": " ++ show passed ++ "/" ++ show (length rs) ++ " passed")
    pure (passed, length rs)
  let passed = sum (map fst results)
      total = sum (map snd results)
  putStrLn ("\n" ++ show passed ++ "/" ++ show total ++ " tests passed")
  if passed /= total then exitFailure else pure ()
  where
    report tname msg = putStrLn ("  FAIL " ++ tname ++ "\n       " ++ msg)
    forceResult r = case r of
      Left msg -> length msg `seq` r
      Right () -> r
