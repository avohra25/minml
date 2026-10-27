-- | Glue: parse, type-check and evaluate programs in a persistent session.
module MiniML.Driver
  ( Session (..)
  , Mode (..)
  , baseSession
  , preludeSession
  , runSource
  , typeOf
  , evalString
  ) where

import qualified Data.Map.Strict as Map
import MiniML.Builtins
import MiniML.Eval
import MiniML.Infer
import MiniML.Parser
import MiniML.Prelude
import MiniML.Syntax
import MiniML.Types
import MiniML.Value

data Session = Session
  { sTypes :: TypeEnv
  , sValues :: Env
  }

data Mode = Check | Run
  deriving (Eq, Show)

-- | Only the primitives.
baseSession :: Session
baseSession = Session builtinTypes builtinValues

-- | Primitives plus the prelude. The prelude is checked like user code;
-- failing to load it is a bug, hence the 'error'.
preludeSession :: Session
preludeSession = case runSource Run baseSession preludeSource of
  Right (s, _) -> s
  Left err -> error ("prelude failed to load: " ++ err)

-- | Process every phrase of a source text, printing OCaml-style
-- @val name : type = value@ lines. Stops at the first error.
runSource :: Mode -> Session -> String -> Either String (Session, [String])
runSource mode session src = do
  decls <- either (Left . ("syntax error: " ++) . renderParseError) Right (parseProgram src)
  go session decls
  where
    go s [] = Right (s, [])
    go s (d : ds) = do
      (s', out) <- runDecl mode s d
      (s'', outs) <- go s' ds
      pure (s'', out ++ outs)

runDecl :: Mode -> Session -> Decl -> Either String (Session, [String])
runDecl mode (Session tys vals) d = do
  dt <- either (Left . ("type error: " ++) . renderTypeError) Right (inferDecl tys d)
  let named = case dt of
        Bound bs -> bs
        Anonymous sc -> [("-", sc)]
      tys' = foldr (\(x, sc) -> if x == "-" then id else Map.insert x sc) tys named
  case mode of
    Check -> pure (Session tys' vals, [describe x sc Nothing | (x, sc) <- named])
    Run -> do
      binds <- either (Left . ("runtime error: " ++) . renderEvalError) Right (evalDecl defaultFuel vals d)
      let vals' = foldr (\(x, v) -> if x == "-" then id else Map.insert x v) vals binds
          out = [describe x sc (lookup x binds) | (x, sc) <- named]
      pure (Session tys' vals', out)
  where
    describe x sc mv =
      (if x == "-" then "- : " else "val " ++ x ++ " : ")
        ++ prettyScheme sc
        ++ maybe "" (\v -> " = " ++ showValue v) mv

-- | The principal type of an expression in a session.
typeOf :: Session -> String -> Either String String
typeOf (Session tys _) src = do
  e <- either (Left . ("syntax error: " ++) . renderParseError) Right (parseExpr src)
  sc <- either (Left . ("type error: " ++) . renderTypeError) Right (inferExpr tys e)
  pure (prettyScheme sc)

-- | Type-check and evaluate an expression with the prelude loaded,
-- returning its type and printed value.
evalString :: String -> Either String (String, String)
evalString src = do
  let Session tys vals = preludeSession
  e <- either (Left . ("syntax error: " ++) . renderParseError) Right (parseExpr src)
  sc <- either (Left . ("type error: " ++) . renderTypeError) Right (inferExpr tys e)
  v <- either (Left . ("runtime error: " ++) . renderEvalError) Right (evalExpr defaultFuel vals e)
  pure (prettyScheme sc, showValue v)
