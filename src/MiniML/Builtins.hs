-- | Primitive functions: their type schemes and implementations.
module MiniML.Builtins
  ( builtins
  , builtinTypes
  , builtinValues
  ) where

import qualified Data.Map.Strict as Map
import MiniML.Syntax (Name)
import MiniML.Types
import MiniML.Value
import Text.Read (readMaybe)

data Builtin = Builtin
  { bName :: Name
  , bScheme :: Scheme
  , bArity :: Int
  , bImpl :: [Value] -> Either EvalError Value
  }

a :: Type
a = TVar 0

b :: Type
b = TVar 1

builtins :: [Builtin]
builtins =
  [ Builtin "not" (Forall [] (tBool ~> tBool)) 1 $ \vs -> case vs of
      [VBool x] -> Right (VBool (not x))
      _ -> stuck "not"
  , Builtin "fst" (Forall [0, 1] (tTuple [a, b] ~> a)) 1 $ \vs -> case vs of
      [VTuple [x, _]] -> Right x
      _ -> stuck "fst"
  , Builtin "snd" (Forall [0, 1] (tTuple [a, b] ~> b)) 1 $ \vs -> case vs of
      [VTuple [_, y]] -> Right y
      _ -> stuck "snd"
  , Builtin "string_of_int" (Forall [] (tInt ~> tString)) 1 $ \vs -> case vs of
      [VInt n] -> Right (VStr (show n))
      _ -> stuck "string_of_int"
  , Builtin "int_of_string" (Forall [] (tString ~> tInt)) 1 $ \vs -> case vs of
      [VStr s] -> maybe (Left (RuntimeError ("int_of_string: invalid integer " ++ show s))) (Right . VInt) (parseInt s)
      _ -> stuck "int_of_string"
  , Builtin "string_of_bool" (Forall [] (tBool ~> tString)) 1 $ \vs -> case vs of
      [VBool x] -> Right (VStr (if x then "true" else "false"))
      _ -> stuck "string_of_bool"
  , Builtin "string_length" (Forall [] (tString ~> tInt)) 1 $ \vs -> case vs of
      [VStr s] -> Right (VInt (fromIntegral (length s)))
      _ -> stuck "string_length"
  , Builtin "failwith" (Forall [0] (tString ~> a)) 1 $ \vs -> case vs of
      [VStr s] -> Left (RuntimeError ("exception: " ++ s))
      _ -> stuck "failwith"
  ]
  where
    stuck n = Left (Stuck ("bad arguments to primitive " ++ n))
    parseInt ('-' : ds) = negate <$> readDigits ds
    parseInt ds = readDigits ds
    readDigits ds
      | not (null ds) && all (`elem` ['0' .. '9']) ds = readMaybe ds
      | otherwise = Nothing

builtinTypes :: Map.Map Name Scheme
builtinTypes = Map.fromList [(bName x, bScheme x) | x <- builtins]

builtinValues :: Env
builtinValues = Map.fromList [(bName x, VPrim (bName x) (bArity x) [] (bImpl x)) | x <- builtins]
