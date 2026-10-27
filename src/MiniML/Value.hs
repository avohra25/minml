-- | Runtime values, evaluation errors, structural comparison and printing.
module MiniML.Value
  ( Value (..)
  , Env
  , EvalError (..)
  , compareValues
  , showValue
  , renderEvalError
  ) where

import qualified Data.Map.Strict as Map
import MiniML.Syntax (Expr, Name, escapeString)

type Env = Map.Map Name Value

data Value
  = VInt !Integer
  | VBool !Bool
  | VStr String
  | VUnit
  | VTuple [Value]
  | VList [Value]
  | VClosure Env Name Expr
  -- | A built-in: name, arity, arguments received so far, implementation.
  | VPrim String Int [Value] ([Value] -> Either EvalError Value)

data EvalError
  = RuntimeError String   -- ^ a legitimate run-time failure (failwith, division by zero, ...)
  | Stuck String          -- ^ a state a well-typed program can never reach
  | OutOfFuel             -- ^ the step budget was exhausted
  deriving (Eq, Show)

renderEvalError :: EvalError -> String
renderEvalError e = case e of
  RuntimeError m -> m
  Stuck m -> "evaluation got stuck (this is a bug): " ++ m
  OutOfFuel -> "step limit exceeded (non-terminating program?)"

-- | Structural ordering. Functions cannot be compared.
compareValues :: Value -> Value -> Either EvalError Ordering
compareValues a b = case (a, b) of
  (VInt x, VInt y) -> Right (compare x y)
  (VBool x, VBool y) -> Right (compare x y)
  (VStr x, VStr y) -> Right (compare x y)
  (VUnit, VUnit) -> Right EQ
  (VTuple xs, VTuple ys) -> lexi xs ys
  (VList xs, VList ys) -> lexi xs ys
  (VClosure {}, _) -> functional
  (VPrim {}, _) -> functional
  (_, VClosure {}) -> functional
  (_, VPrim {}) -> functional
  _ -> Left (Stuck "comparison of values of different types")
  where
    functional = Left (RuntimeError "compare: functional value")
    lexi [] [] = Right EQ
    lexi [] _ = Right LT
    lexi _ [] = Right GT
    lexi (x : xs) (y : ys) = do
      o <- compareValues x y
      if o == EQ then lexi xs ys else Right o

showValue :: Value -> String
showValue v = case v of
  VInt n -> show n
  VBool True -> "true"
  VBool False -> "false"
  VStr s -> escapeString s
  VUnit -> "()"
  VTuple vs -> "(" ++ joinWith ", " (map showValue vs) ++ ")"
  VList vs -> "[" ++ joinWith "; " (map showValue vs) ++ "]"
  VClosure {} -> "<fun>"
  VPrim {} -> "<fun>"
  where
    joinWith _ [] = ""
    joinWith sep (x : xs) = x ++ concatMap (sep ++) xs
