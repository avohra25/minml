-- | Hindley-Milner type inference (Algorithm W with a threaded
-- substitution) with let-polymorphism, recursive bindings, tuples, lists
-- and pattern matching.
module MiniML.Infer
  ( TypeEnv
  , TypeError (..)
  , TypeErrorKind (..)
  , DeclType (..)
  , inferExpr
  , inferDecl
  , renderTypeError
  ) where

import Control.Monad (forM, forM_)
import Data.List (group, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MiniML.Syntax
import MiniML.Types

type TypeEnv = Map.Map Name Scheme

data TypeErrorKind
  = Mismatch Type Type              -- ^ expected, actual
  | PatternMismatch Type Type       -- ^ scrutinee type, pattern type
  | InfiniteType TVar Type
  | NotAFunction Type
  | Unbound Name
  | LetRecNotFunction Name
  | DuplicatePatVar Name
  deriving (Eq, Show)

data TypeError = TypeError { errPos :: Maybe Pos, errKind :: TypeErrorKind }
  deriving (Eq, Show)

renderTypeError :: TypeError -> String
renderTypeError (TypeError mpos k) = loc ++ msg
  where
    loc = maybe "" (\(Pos l c) -> "line " ++ show l ++ ", column " ++ show c ++ ": ") mpos
    msg = case k of
      Mismatch e a ->
        let (e', a') = pretty2 e a
        in "this expression has type " ++ a' ++ " but an expression was expected of type " ++ e'
      PatternMismatch s p ->
        let (s', p') = pretty2 s p
        in "this pattern matches values of type " ++ p' ++ " but the matched value has type " ++ s'
      InfiniteType v t ->
        let (v', t') = pretty2 (TVar v) t
        in "cannot construct the infinite type " ++ v' ++ " = " ++ t'
      NotAFunction t -> "this expression has type " ++ prettyType t ++ "; it is not a function and cannot be applied"
      Unbound x -> "unbound variable '" ++ x ++ "'"
      LetRecNotFunction f -> "the right-hand side of 'let rec " ++ f ++ "' must be a function"
      DuplicatePatVar x -> "variable '" ++ x ++ "' is bound several times in this pattern"

-- | Pretty-print two types with a shared type-variable naming.
pretty2 :: Type -> Type -> (String, String)
pretty2 x y = case prettyTypes [x, y] of
  [x', y'] -> (x', y')
  _ -> (prettyType x, prettyType y)

----------------------------------------------------------------------
-- The inference monad: fresh-variable supply + current substitution.

data St = St { supply :: !Int, subst :: !Subst }

newtype Infer a = Infer { runInfer :: St -> Either TypeError (a, St) }

instance Functor Infer where
  fmap f (Infer m) = Infer $ \s -> fmap (\(a, s') -> (f a, s')) (m s)

instance Applicative Infer where
  pure a = Infer $ \s -> Right (a, s)
  mf <*> ma = do { f <- mf; f <$> ma }

instance Monad Infer where
  Infer m >>= k = Infer $ \s -> case m s of
    Left e -> Left e
    Right (a, s') -> runInfer (k a) s'

throw :: TypeErrorKind -> Infer a
throw k = Infer $ \_ -> Left (TypeError Nothing k)

-- | Attach a position to errors raised inside, unless a more precise
-- (inner) position is already present.
withPos :: Maybe Pos -> Infer a -> Infer a
withPos Nothing m = m
withPos (Just p) (Infer m) = Infer $ \s -> case m s of
  Left (TypeError Nothing k) -> Left (TypeError (Just p) k)
  r -> r

fresh :: Infer Type
fresh = Infer $ \s -> Right (TVar (supply s), s { supply = supply s + 1 })

currentSubst :: Infer Subst
currentSubst = Infer $ \s -> Right (subst s, s)

zonk :: Types a => a -> Infer a
zonk t = (`apply` t) <$> currentSubst

extendSubst :: Subst -> Infer ()
extendSubst s1 = Infer $ \s -> Right ((), s { subst = s1 `compose` subst s })

----------------------------------------------------------------------
-- Unification

data MguError = Clash | Occurs TVar Type

mgu :: Type -> Type -> Either MguError Subst
mgu (TVar a) t = bindVar a t
mgu t (TVar a) = bindVar a t
mgu (TCon c1 as) (TCon c2 bs)
  | c1 == c2 && length as == length bs = go as bs
  | otherwise = Left Clash
  where
    go [] [] = Right emptySubst
    go (x : xs) (y : ys) = do
      s1 <- mgu x y
      s2 <- go (apply s1 xs) (apply s1 ys)
      pure (s2 `compose` s1)
    go _ _ = Left Clash

bindVar :: TVar -> Type -> Either MguError Subst
bindVar a t
  | t == TVar a = Right emptySubst
  | a `Set.member` ftv t = Left (Occurs a t)
  | otherwise = Right (Map.singleton a t)

-- | Unify with a custom error for a clash between the full types.
unifyWith :: (Type -> Type -> TypeErrorKind) -> Type -> Type -> Infer ()
unifyWith mk expected actual = do
  e <- zonk expected
  a <- zonk actual
  case mgu e a of
    Right s -> extendSubst s
    Left Clash -> throw (mk e a)
    Left (Occurs v t) -> throw (InfiniteType v t)

-- | @unifyAt e expected actual@: the expression @e@ has type @actual@
-- but its context needs @expected@. Errors point at @e@.
unifyAt :: Expr -> Type -> Type -> Infer ()
unifyAt e expected actual = withPos (exprPos e) (unifyWith Mismatch expected actual)

instantiate :: Scheme -> Infer Type
instantiate (Forall vs t) = do
  fs <- mapM (const fresh) vs
  pure (apply (Map.fromList (zip vs fs)) t)

generalize :: TypeEnv -> Type -> Infer Scheme
generalize env t = do
  env' <- zonk env
  t' <- zonk t
  let vs = Set.toList (ftv t' `Set.difference` ftv env')
  pure (Forall vs t')

----------------------------------------------------------------------
-- Expressions

litType :: Lit -> Type
litType l = case l of
  LInt _ -> tInt
  LBool _ -> tBool
  LStr _ -> tString
  LUnit -> tUnit

-- | Argument types and result type of a binary operator.
opType :: BinOp -> Infer (Type, Type, Type)
opType op
  | op `elem` [Add, Sub, Mul, Div, Mod] = pure (tInt, tInt, tInt)
  | op `elem` [Eq, Neq, Lt, Le, Gt, Ge] = do { a <- fresh; pure (a, a, tBool) }
  | op `elem` [And, Or] = pure (tBool, tBool, tBool)
  | op == Concat = pure (tString, tString, tString)
  | op == Cons = do { a <- fresh; pure (a, tList a, tList a) }
  | otherwise = do { a <- fresh; pure (tList a, tList a, tList a) } -- Append

infer :: TypeEnv -> Expr -> Infer Type
infer env ex = case ex of
  EAt p e -> withPos (Just p) (infer env e)
  ELit l -> pure (litType l)
  EVar x -> maybe (throw (Unbound x)) instantiate (Map.lookup x env)
  ELam x body -> do
    a <- fresh
    r <- infer (Map.insert x (Forall [] a) env) body
    pure (a ~> r)
  EApp f arg -> do
    tf <- infer env f >>= zonk
    (targ, tres) <- case tf of
      TCon "->" [a, r] -> pure (a, r)
      TVar _ -> do
        a <- fresh
        r <- fresh
        unifyAt f (a ~> r) tf
        pure (a, r)
      _ -> withPos (exprPos f) (throw (NotAFunction tf))
    ta <- infer env arg
    unifyAt arg targ ta
    pure tres
  ELet x rhs body -> do
    t1 <- infer env rhs
    sc <- generalize env t1
    infer (Map.insert x sc env) body
  ELetRec f rhs body -> do
    sc <- inferRec env f rhs
    infer (Map.insert f sc env) body
  EIf c a b -> do
    tc <- infer env c
    unifyAt c tBool tc
    ta <- infer env a
    tb <- infer env b
    unifyAt b ta tb
    pure ta
  EBin op a b -> do
    (ea, eb, r) <- opType op
    ta <- infer env a
    unifyAt a ea ta
    tb <- infer env b
    unifyAt b eb tb
    pure r
  ETuple es -> tTuple <$> mapM (infer env) es
  EList es -> do
    a <- fresh
    forM_ es $ \e -> infer env e >>= unifyAt e a
    pure (tList a)
  EMatch scrut arms -> do
    ts <- infer env scrut
    r <- fresh
    forM_ arms $ \(p, body) -> do
      (tp, binds) <- inferPat p
      withPos (exprPos scrut) (unifyWith PatternMismatch ts tp)
      tb <- infer (foldr (\(x, t) -> Map.insert x (Forall [] t)) env binds) body
      unifyAt body r tb
    pure r

inferRec :: TypeEnv -> Name -> Expr -> Infer Scheme
inferRec env f rhs = do
  case stripAt rhs of
    ELam _ _ -> pure ()
    _ -> withPos (exprPos rhs) (throw (LetRecNotFunction f))
  a <- fresh
  t <- infer (Map.insert f (Forall [] a) env) rhs
  unifyAt rhs a t
  generalize env t

inferPat :: Pat -> Infer (Type, [(Name, Type)])
inferPat p0 = do
  let dups = [x | (x : _ : _) <- group (sort (patVars p0))]
  forM_ (take 1 dups) (throw . DuplicatePatVar)
  go p0
  where
    go p = case p of
      PWild -> (\t -> (t, [])) <$> fresh
      PVar x -> (\t -> (t, [(x, t)])) <$> fresh
      PLit l -> pure (litType l, [])
      PNil -> (\t -> (tList t, [])) <$> fresh
      PCons h t -> do
        (th, bh) <- go h
        (tt, bt) <- go t
        unifyWith PatternMismatch (tList th) tt
        pure (tt, bh ++ bt)
      PTuple ps -> do
        rs <- forM ps go
        pure (tTuple (map fst rs), concatMap snd rs)

----------------------------------------------------------------------
-- Entry points

runFrom :: Infer a -> Either TypeError a
runFrom m = fst <$> runInfer m (St 0 emptySubst)

-- | Infer the principal type scheme of a closed (w.r.t. the environment)
-- expression.
inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env e = runFrom $ do
  t <- infer env e
  normalizeScheme <$> generalize env t

data DeclType
  = Bound [(Name, Scheme)]   -- ^ names introduced by a @let@
  | Anonymous Scheme         -- ^ type of a bare expression
  deriving (Eq, Show)

inferDecl :: TypeEnv -> Decl -> Either TypeError DeclType
inferDecl env d = runFrom $ case d of
  DExpr e -> do
    t <- infer env e
    Anonymous . normalizeScheme <$> generalize env t
  DLetRec f rhs -> do
    sc <- inferRec env f rhs
    pure (Bound [(f, normalizeScheme sc)])
  DLet p rhs -> do
    t <- infer env rhs
    (tp, binds) <- inferPat p
    withPos (exprPos rhs) (unifyWith PatternMismatch t tp)
    scs <- forM binds $ \(x, tx) -> do
      sc <- generalize env tx
      pure (x, normalizeScheme sc)
    pure (Bound scs)
