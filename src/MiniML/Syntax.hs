-- | Abstract syntax of MiniML and a fully parenthesised pretty-printer.
module MiniML.Syntax
  ( Name
  , Pos (..)
  , Lit (..)
  , BinOp (..)
  , Pat (..)
  , Expr (..)
  , Decl (..)
  , stripAt
  , stripPos
  , exprPos
  , patVars
  , binOpSymbol
  , prettyExpr
  , prettyPat
  , prettyLit
  , escapeString
  ) where

type Name = String

-- | A source position (1-based line and column).
data Pos = Pos { posLine :: !Int, posCol :: !Int }
  deriving (Eq, Ord, Show)

data Lit
  = LInt Integer
  | LBool Bool
  | LStr String
  | LUnit
  deriving (Eq, Show)

data BinOp
  = Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Le | Gt | Ge
  | And | Or
  | Cons | Append | Concat
  deriving (Eq, Show, Enum, Bounded)

data Pat
  = PWild
  | PVar Name
  | PLit Lit
  | PNil
  | PCons Pat Pat
  | PTuple [Pat]
  deriving (Eq, Show)

data Expr
  = EVar Name
  | ELit Lit
  | EApp Expr Expr
  | ELam Name Expr
  | ELet Name Expr Expr
  | ELetRec Name Expr Expr
  | EIf Expr Expr Expr
  | EBin BinOp Expr Expr
  | ETuple [Expr]
  | EList [Expr]
  | EMatch Expr [(Pat, Expr)]
  | EAt Pos Expr            -- ^ source position annotation
  deriving (Eq, Show)

-- | A top-level phrase.
data Decl
  = DLet Pat Expr           -- ^ @let p = e@
  | DLetRec Name Expr       -- ^ @let rec f = fun ...@
  | DExpr Expr              -- ^ a bare expression
  deriving (Eq, Show)

-- | Remove position annotations at the root.
stripAt :: Expr -> Expr
stripAt (EAt _ e) = stripAt e
stripAt e = e

-- | Remove every position annotation in the tree.
stripPos :: Expr -> Expr
stripPos ex = case ex of
  EAt _ e -> stripPos e
  EVar _ -> ex
  ELit _ -> ex
  EApp f a -> EApp (stripPos f) (stripPos a)
  ELam x b -> ELam x (stripPos b)
  ELet x a b -> ELet x (stripPos a) (stripPos b)
  ELetRec x a b -> ELetRec x (stripPos a) (stripPos b)
  EIf c a b -> EIf (stripPos c) (stripPos a) (stripPos b)
  EBin o a b -> EBin o (stripPos a) (stripPos b)
  ETuple es -> ETuple (map stripPos es)
  EList es -> EList (map stripPos es)
  EMatch s arms -> EMatch (stripPos s) [(p, stripPos e) | (p, e) <- arms]

-- | The outermost recorded position of an expression, if any.
exprPos :: Expr -> Maybe Pos
exprPos (EAt p _) = Just p
exprPos _ = Nothing

-- | Variables bound by a pattern, left to right (duplicates included).
patVars :: Pat -> [Name]
patVars p = case p of
  PVar x -> [x]
  PCons h t -> patVars h ++ patVars t
  PTuple ps -> concatMap patVars ps
  _ -> []

binOpSymbol :: BinOp -> String
binOpSymbol o = case o of
  Add -> "+"; Sub -> "-"; Mul -> "*"; Div -> "/"; Mod -> "%"
  Eq -> "="; Neq -> "<>"; Lt -> "<"; Le -> "<="; Gt -> ">"; Ge -> ">="
  And -> "&&"; Or -> "||"
  Cons -> "::"; Append -> "@"; Concat -> "^"

escapeString :: String -> String
escapeString s = '"' : concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc c = [c]

prettyLit :: Lit -> String
prettyLit l = case l of
  LInt n | n < 0 -> "(" ++ show n ++ ")"
         | otherwise -> show n
  LBool True -> "true"
  LBool False -> "false"
  LStr s -> escapeString s
  LUnit -> "()"

prettyPat :: Pat -> String
prettyPat p = case p of
  PWild -> "_"
  PVar x -> x
  PLit (LInt n) -> show n
  PLit l -> prettyLit l
  PNil -> "[]"
  PCons h t -> "(" ++ prettyPat h ++ " :: " ++ prettyPat t ++ ")"
  PTuple ps -> "(" ++ commaSep (map prettyPat ps) ++ ")"

-- | Print an expression with explicit parentheses everywhere, so that
-- re-parsing the output yields the same tree (modulo positions).
prettyExpr :: Expr -> String
prettyExpr ex = case ex of
  EAt _ e -> prettyExpr e
  EVar x -> x
  ELit l -> prettyLit l
  EApp f a -> "(" ++ prettyExpr f ++ " " ++ prettyExpr a ++ ")"
  ELam x b -> "(fun " ++ x ++ " -> " ++ prettyExpr b ++ ")"
  ELet x a b -> "(let " ++ x ++ " = " ++ prettyExpr a ++ " in " ++ prettyExpr b ++ ")"
  ELetRec x a b -> "(let rec " ++ x ++ " = " ++ prettyExpr a ++ " in " ++ prettyExpr b ++ ")"
  EIf c a b -> "(if " ++ prettyExpr c ++ " then " ++ prettyExpr a ++ " else " ++ prettyExpr b ++ ")"
  EBin o a b -> "(" ++ prettyExpr a ++ " " ++ binOpSymbol o ++ " " ++ prettyExpr b ++ ")"
  ETuple es -> "(" ++ commaSep (map prettyExpr es) ++ ")"
  EList es -> "[" ++ joinWith "; " (map prettyExpr es) ++ "]"
  EMatch s arms ->
    "(match " ++ prettyExpr s ++ " with "
      ++ joinWith " | " [prettyPat p ++ " -> (" ++ prettyExpr e ++ ")" | (p, e) <- arms]
      ++ ")"

commaSep :: [String] -> String
commaSep = joinWith ", "

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x : xs) = x ++ sep ++ joinWith sep xs
