-- | Recursive-descent / precedence-climbing parser for MiniML.
--
-- Operator precedence, loosest first (all binary operators are
-- left-associative unless noted):
--
-- > let, fun, if, match     extend as far right as possible
-- > ||                      right
-- > &&                      right
-- > = <> < <= > >=
-- > :: @ ^                  right
-- > + -
-- > * / %
-- > unary -
-- > function application
module MiniML.Parser
  ( ParseError (..)
  , parseProgram
  , parseExpr
  , renderParseError
  ) where

import MiniML.Lexer
import MiniML.Syntax

data ParseError = ParseError Pos String
  deriving (Eq, Show)

renderParseError :: ParseError -> String
renderParseError (ParseError (Pos l c) msg) =
  "line " ++ show l ++ ", column " ++ show c ++ ": " ++ msg

newtype P a = P { runP :: [Token] -> Either ParseError (a, [Token]) }

instance Functor P where
  fmap f (P p) = P $ \ts -> case p ts of
    Left e -> Left e
    Right (a, ts') -> Right (f a, ts')

instance Applicative P where
  pure a = P $ \ts -> Right (a, ts)
  pf <*> pa = do { f <- pf; f <$> pa }

instance Monad P where
  P p >>= k = P $ \ts -> case p ts of
    Left e -> Left e
    Right (a, ts') -> runP (k a) ts'

peekToken :: P Token
peekToken = P $ \ts -> case ts of
  (t : _) -> Right (t, ts)
  [] -> Left (ParseError (Pos 0 0) "internal error: token stream exhausted")

peek :: P Tok
peek = tokVal <$> peekToken

currentPos :: P Pos
currentPos = tokPos <$> peekToken

advance :: P Token
advance = P $ \ts -> case ts of
  (t : rest) | tokVal t /= TEOF -> Right (t, rest)
  (t : _) -> Right (t, ts)
  [] -> Left (ParseError (Pos 0 0) "internal error: token stream exhausted")

failHere :: String -> P a
failHere expected = do
  t <- peekToken
  P $ \_ -> Left (ParseError (tokPos t) ("expected " ++ expected ++ " but found " ++ showTok (tokVal t)))

expect :: Tok -> String -> P ()
expect tok what = do
  t <- peek
  if t == tok then () <$ advance else failHere what

optional' :: Tok -> P Bool
optional' tok = do
  t <- peek
  if t == tok then True <$ advance else pure False

sym :: String -> Tok
sym = TSym

kw :: String -> Tok
kw = TKw

----------------------------------------------------------------------
-- Entry points

parseWith :: P a -> String -> Either ParseError a
parseWith p src = case lexML src of
  Left (LexError pos msg) -> Left (ParseError pos msg)
  Right toks -> fst <$> runP (p <* eof) toks
  where
    eof = do
      t <- peek
      if t == TEOF then pure () else failHere "end of input"

-- | Parse a sequence of top-level phrases, optionally separated by @;;@.
parseProgram :: String -> Either ParseError [Decl]
parseProgram = parseWith program

-- | Parse a single expression.
parseExpr :: String -> Either ParseError Expr
parseExpr = parseWith expr

program :: P [Decl]
program = do
  skipSemis
  t <- peek
  if t == TEOF
    then pure []
    else do
      d <- decl
      t' <- peek
      if t' `notElem` [TEOF, sym ";;", kw "let"] && not (startsExpr t')
        then failHere "a new declaration or ';;'"
        else (d :) <$> program
  where
    skipSemis = do
      more <- optional' (sym ";;")
      if more then skipSemis else pure ()

decl :: P Decl
decl = do
  t <- peek
  case t of
    TKw "let" -> do
      pos <- currentPos
      _ <- advance
      isRec <- optional' (kw "rec")
      (binder, rhs) <- letBinding isRec
      hasIn <- optional' (kw "in")
      if hasIn
        then do
          body <- expr
          pure (DExpr (EAt pos (mkLet isRec binder rhs body)))
        else pure $ case binder of
          PVar f | isRec -> DLetRec f rhs
          _ -> DLet binder rhs
    _ -> DExpr <$> expr

-- | Parses @p = e@ or @f p1 ... pn = e@ (after @let@ / @let rec@).
letBinding :: Bool -> P (Pat, Expr)
letBinding isRec = do
  binder <- patAtom
  params <- many' startsPatAtom patAtom
  case (binder, params) of
    (PVar _, _) -> pure ()
    (_, []) | not isRec -> pure ()
    (_, []) -> failHere "a function name after 'let rec'"
    _ -> failHere "'=' (only a plain name can take parameters)"
  expect (sym "=") "'='"
  body <- expr
  pure (binder, foldr mkLam body params)

mkLet :: Bool -> Pat -> Expr -> Expr -> Expr
mkLet True (PVar f) rhs body = ELetRec f rhs body
mkLet _ (PVar x) rhs body = ELet x rhs body
mkLet _ p rhs body = EMatch rhs [(p, body)]

-- | @fun p -> e@ where non-variable patterns are matched immediately.
mkLam :: Pat -> Expr -> Expr
mkLam (PVar x) body = ELam x body
mkLam PWild body = ELam "_" body
mkLam p body = ELam "%arg" (EMatch (EVar "%arg") [(p, body)])

many' :: (Tok -> Bool) -> P a -> P [a]
many' starts p = do
  t <- peek
  if starts t then (:) <$> p <*> many' starts p else pure []

----------------------------------------------------------------------
-- Expressions

startsKwExpr :: Tok -> Bool
startsKwExpr t = t `elem` map kw ["let", "fun", "if", "match"]

startsAtom :: Tok -> Bool
startsAtom t = case t of
  TInt _ -> True
  TStr _ -> True
  TIdent _ -> True
  TKw k -> k `elem` ["true", "false"]
  TSym s -> s `elem` ["(", "["]
  TEOF -> False

startsExpr :: Tok -> Bool
startsExpr t = startsAtom t || startsKwExpr t || t == sym "-"

expr :: P Expr
expr = do
  t <- peek
  pos <- currentPos
  case t of
    TKw "let" -> do
      _ <- advance
      isRec <- optional' (kw "rec")
      (binder, rhs) <- letBinding isRec
      expect (kw "in") "'in'"
      body <- expr
      pure (EAt pos (mkLet isRec binder rhs body))
    TKw "fun" -> do
      _ <- advance
      params <- many' startsPatAtom patAtom
      if null params then failHere "a parameter" else pure ()
      expect (sym "->") "'->'"
      body <- expr
      pure (EAt pos (foldr mkLam body params))
    TKw "if" -> do
      _ <- advance
      c <- expr
      expect (kw "then") "'then'"
      a <- expr
      expect (kw "else") "'else'"
      b <- expr
      pure (EAt pos (EIf c a b))
    TKw "match" -> do
      _ <- advance
      scrut <- expr
      expect (kw "with") "'with'"
      _ <- optional' (sym "|")
      arms <- matchArms
      pure (EAt pos (EMatch scrut arms))
    _ -> binary 0

matchArms :: P [(Pat, Expr)]
matchArms = do
  p <- pattern
  expect (sym "->") "'->'"
  body <- expr
  more <- optional' (sym "|")
  if more then ((p, body) :) <$> matchArms else pure [(p, body)]

data Assoc = LeftA | RightA

levels :: [(Assoc, [(String, BinOp)])]
levels =
  [ (RightA, [("||", Or)])
  , (RightA, [("&&", And)])
  , (LeftA, [("=", Eq), ("<>", Neq), ("<", Lt), ("<=", Le), (">", Gt), (">=", Ge)])
  , (RightA, [("::", Cons), ("@", Append), ("^", Concat)])
  , (LeftA, [("+", Add), ("-", Sub)])
  , (LeftA, [("*", Mul), ("/", Div), ("%", Mod)])
  ]

-- | An operand on the right of an operator may be a keyword expression,
-- e.g. @1 + if b then 2 else 3@.
operand :: P Expr -> P Expr
operand p = do
  t <- peek
  if startsKwExpr t then expr else p

binary :: Int -> P Expr
binary i
  | i >= length levels = unary
  | otherwise = binary (i + 1) >>= continue
  where
    (assoc, ops) = levels !! i
    next = binary (i + 1)
    opAt = do
      t <- peekToken
      pure $ case tokVal t of
        TSym s -> (,) (tokPos t) <$> lookup s ops
        _ -> Nothing
    continue lhs = do
      m <- opAt
      case m of
        Nothing -> pure lhs
        Just (pos, op) -> do
          _ <- advance
          case assoc of
            RightA -> do
              rhs <- operand (binary i)
              pure (EAt pos (EBin op lhs rhs))
            LeftA -> do
              rhs <- operand next
              continue (EAt pos (EBin op lhs rhs))

unary :: P Expr
unary = do
  t <- peek
  if t == sym "-"
    then do
      pos <- currentPos
      _ <- advance
      e <- operand unary
      pure $ case stripAt e of
        ELit (LInt n) -> EAt pos (ELit (LInt (negate n)))
        _ -> EAt pos (EBin Sub (ELit (LInt 0)) e)
    else application

application :: P Expr
application = do
  pos <- currentPos
  f <- atom
  args <- many' startsAtom atom
  pure (foldl (\acc a -> EAt pos (EApp acc a)) f args)

atom :: P Expr
atom = do
  tk <- peekToken
  let pos = tokPos tk
  case tokVal tk of
    TInt n -> advance >> pure (EAt pos (ELit (LInt n)))
    TStr s -> advance >> pure (EAt pos (ELit (LStr s)))
    TKw "true" -> advance >> pure (EAt pos (ELit (LBool True)))
    TKw "false" -> advance >> pure (EAt pos (ELit (LBool False)))
    TIdent x -> advance >> pure (EAt pos (EVar x))
    TSym "(" -> do
      _ <- advance
      closed <- optional' (sym ")")
      if closed
        then pure (EAt pos (ELit LUnit))
        else do
          e <- expr
          rest <- many' (== sym ",") (advance >> expr)
          expect (sym ")") "')'"
          pure (if null rest then e else EAt pos (ETuple (e : rest)))
    TSym "[" -> do
      _ <- advance
      closed <- optional' (sym "]")
      if closed then pure (EAt pos (EList [])) else do
        es <- listElems
        pure (EAt pos (EList es))
    _ -> failHere "an expression"
  where
    listElems = do
      e <- expr
      semi <- optional' (sym ";")
      t <- peek
      if t == sym "]"
        then advance >> pure [e]
        else if semi then (e :) <$> listElems else failHere "';' or ']'"

----------------------------------------------------------------------
-- Patterns

startsPatAtom :: Tok -> Bool
startsPatAtom t = case t of
  TInt _ -> True
  TStr _ -> True
  TIdent _ -> True
  TKw k -> k `elem` ["true", "false"]
  TSym s -> s `elem` ["(", "[", "_"]
  TEOF -> False

pattern :: P Pat
pattern = do
  p <- patAtom
  isCons <- optional' (sym "::")
  if isCons then PCons p <$> pattern else pure p

patAtom :: P Pat
patAtom = do
  t <- peek
  case t of
    TSym "_" -> advance >> pure PWild
    TIdent x -> advance >> pure (PVar x)
    TInt n -> advance >> pure (PLit (LInt n))
    TSym "-" -> do
      _ <- advance
      t' <- peek
      case t' of
        TInt n -> advance >> pure (PLit (LInt (negate n)))
        _ -> failHere "an integer after '-' in a pattern"
    TStr s -> advance >> pure (PLit (LStr s))
    TKw "true" -> advance >> pure (PLit (LBool True))
    TKw "false" -> advance >> pure (PLit (LBool False))
    TSym "(" -> do
      _ <- advance
      closed <- optional' (sym ")")
      if closed then pure (PLit LUnit) else do
        p <- pattern
        rest <- many' (== sym ",") (advance >> pattern)
        expect (sym ")") "')'"
        pure (if null rest then p else PTuple (p : rest))
    TSym "[" -> do
      _ <- advance
      closed <- optional' (sym "]")
      if closed then pure PNil else do
        ps <- patElems
        pure (foldr PCons PNil ps)
    _ -> failHere "a pattern"
  where
    patElems = do
      p <- pattern
      semi <- optional' (sym ";")
      t <- peek
      if t == sym "]"
        then advance >> pure [p]
        else if semi then (p :) <$> patElems else failHere "';' or ']'"
