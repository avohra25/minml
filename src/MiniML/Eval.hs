-- | A call-by-value, environment-based interpreter with a step budget.
module MiniML.Eval
  ( evalExpr
  , evalDecl
  , defaultFuel
  ) where

import qualified Data.Map.Strict as Map
import MiniML.Syntax
import MiniML.Value

-- | State-passing monad threading the remaining step budget.
newtype Eval a = Eval { runEval :: Int -> Either EvalError (a, Int) }

instance Functor Eval where
  fmap f (Eval m) = Eval $ \n -> fmap (\(x, n') -> (f x, n')) (m n)

instance Applicative Eval where
  pure x = Eval $ \n -> Right (x, n)
  mf <*> mx = do { f <- mf; f <$> mx }

instance Monad Eval where
  Eval m >>= k = Eval $ \n -> case m n of
    Left e -> Left e
    Right (x, n') -> runEval (k x) n'

failE :: EvalError -> Eval a
failE e = Eval $ \_ -> Left e

liftE :: Either EvalError a -> Eval a
liftE = either failE pure

tick :: Eval ()
tick = Eval $ \n -> if n <= 0 then Left OutOfFuel else Right ((), n - 1)

stuck :: String -> Eval a
stuck = failE . Stuck

defaultFuel :: Int
defaultFuel = 20000000

eval :: Env -> Expr -> Eval Value
eval env ex = case ex of
  EAt _ e -> eval env e
  ELit l -> pure (litValue l)
  EVar x -> maybe (stuck ("unbound variable " ++ x)) pure (Map.lookup x env)
  ELam x body -> pure (VClosure env x body)
  EApp f arg -> do
    fv <- eval env f
    av <- eval env arg
    applyValue fv av
  ELet x rhs body -> do
    v <- eval env rhs
    eval (Map.insert x v env) body
  ELetRec f rhs body -> do
    env' <- recEnv env f rhs
    eval env' body
  EIf c a b -> do
    cv <- eval env c
    case cv of
      VBool True -> eval env a
      VBool False -> eval env b
      _ -> stuck "if condition is not a boolean"
  EBin And a b -> do
    av <- eval env a
    case av of
      VBool True -> eval env b
      VBool False -> pure (VBool False)
      _ -> stuck "&& on a non-boolean"
  EBin Or a b -> do
    av <- eval env a
    case av of
      VBool True -> pure (VBool True)
      VBool False -> eval env b
      _ -> stuck "|| on a non-boolean"
  EBin op a b -> do
    av <- eval env a
    bv <- eval env b
    binop op av bv
  ETuple es -> VTuple <$> mapM (eval env) es
  EList es -> VList <$> mapM (eval env) es
  EMatch scrut arms -> do
    v <- eval env scrut
    matchArms env v arms

-- | Build the environment for @let rec f = fun x -> body@ by tying the knot.
recEnv :: Env -> Name -> Expr -> Eval Env
recEnv env f rhs = case stripAt rhs of
  ELam x body ->
    let env' = Map.insert f (VClosure env' x body) env
    in pure env'
  _ -> stuck "let rec of a non-function"

litValue :: Lit -> Value
litValue l = case l of
  LInt n -> VInt n
  LBool x -> VBool x
  LStr s -> VStr s
  LUnit -> VUnit

applyValue :: Value -> Value -> Eval Value
applyValue fv av = case fv of
  VClosure cenv x body -> do
    tick
    eval (Map.insert x av cenv) body
  VPrim name arity args impl -> do
    tick
    let args' = args ++ [av]
    if length args' == arity
      then liftE (impl args')
      else pure (VPrim name arity args' impl)
  _ -> stuck "application of a non-function"

binop :: BinOp -> Value -> Value -> Eval Value
binop op a b = case (op, a, b) of
  (Add, VInt x, VInt y) -> int (x + y)
  (Sub, VInt x, VInt y) -> int (x - y)
  (Mul, VInt x, VInt y) -> int (x * y)
  (Div, VInt _, VInt 0) -> failE (RuntimeError "division by zero")
  (Div, VInt x, VInt y) -> int (x `quot` y)
  (Mod, VInt _, VInt 0) -> failE (RuntimeError "division by zero")
  (Mod, VInt x, VInt y) -> int (x `rem` y)
  (Concat, VStr x, VStr y) -> pure (VStr (x ++ y))
  (Cons, x, VList xs) -> pure (VList (x : xs))
  (Append, VList xs, VList ys) -> pure (VList (xs ++ ys))
  _ | op `elem` [Eq, Neq, Lt, Le, Gt, Ge] -> do
        o <- liftE (compareValues a b)
        pure . VBool $ case op of
          Eq -> o == EQ
          Neq -> o /= EQ
          Lt -> o == LT
          Le -> o /= GT
          Gt -> o == GT
          _ -> o /= LT
  _ -> stuck ("bad operands for " ++ binOpSymbol op)
  where
    int = pure . VInt

matchArms :: Env -> Value -> [(Pat, Expr)] -> Eval Value
matchArms _ v [] = failE (RuntimeError ("match failure: no case matches " ++ showValue v))
matchArms env v ((p, body) : rest) = do
  m <- matchPat p v
  case m of
    Just binds -> eval (foldr (uncurry Map.insert) env binds) body
    Nothing -> matchArms env v rest

-- | Nothing = the pattern does not match; failure = ill-typed match.
matchPat :: Pat -> Value -> Eval (Maybe [(Name, Value)])
matchPat p v = case (p, v) of
  (PWild, _) -> yes []
  (PVar x, _) -> yes [(x, v)]
  (PLit (LInt n), VInt m) -> pure (if n == m then Just [] else Nothing)
  (PLit (LBool x), VBool y) -> pure (if x == y then Just [] else Nothing)
  (PLit (LStr s), VStr t) -> pure (if s == t then Just [] else Nothing)
  (PLit LUnit, VUnit) -> yes []
  (PNil, VList xs) -> pure (if null xs then Just [] else Nothing)
  (PCons _ _, VList []) -> pure Nothing
  (PCons ph pt, VList (x : xs)) -> both [(ph, x), (pt, VList xs)]
  (PTuple ps, VTuple vs) | length ps == length vs -> both (zip ps vs)
  _ -> stuck "pattern does not fit the shape of the value"
  where
    yes = pure . Just
    both [] = yes []
    both ((q, w) : more) = do
      m <- matchPat q w
      case m of
        Nothing -> pure Nothing
        Just bs -> fmap (bs ++) <$> both more

-- | Evaluate an expression with a step budget.
evalExpr :: Int -> Env -> Expr -> Either EvalError Value
evalExpr fuel env e = fst <$> runEval (eval env e) fuel

-- | Evaluate a top-level declaration, returning the new bindings (or the
-- value of a bare expression under the name @-@).
evalDecl :: Int -> Env -> Decl -> Either EvalError [(Name, Value)]
evalDecl fuel env d = fst <$> runEval go fuel
  where
    go = case d of
      DExpr e -> (\v -> [("-", v)]) <$> eval env e
      DLetRec f rhs -> do
        env' <- recEnv env f rhs
        maybe (stuck "let rec") (\v -> pure [(f, v)]) (Map.lookup f env')
      DLet p rhs -> do
        v <- eval env rhs
        m <- matchPat p v
        maybe (failE (RuntimeError ("match failure: the pattern does not match " ++ showValue v))) pure m
