module ParserSpec (parserTests) where

import Harness
import MiniML.Parser
import MiniML.Syntax

pe :: String -> Either String Expr
pe = either (Left . renderParseError) (Right . stripPos) . parseExpr

pp :: String -> Either String [Decl]
pp = either (Left . renderParseError) Right . parseProgram

int :: Integer -> Expr
int = ELit . LInt

v :: Name -> Expr
v = EVar

parserTests :: (String, [Test])
parserTests = group "parser"
  [ test "integer literal" $ assertEq (Right (int 42)) (pe "42")
  , test "negative literal folds" $ assertEq (Right (int (-7))) (pe "-7")
  , test "string escapes" $ assertEq (Right (ELit (LStr "a\"b\n"))) (pe "\"a\\\"b\\n\"")
  , test "application is left-associative" $
      assertEq (Right (EApp (EApp (v "f") (v "x")) (v "y"))) (pe "f x y")
  , test "* binds tighter than +" $
      assertEq (Right (EBin Add (int 1) (EBin Mul (int 2) (int 3)))) (pe "1 + 2 * 3")
  , test "- is left-associative" $
      assertEq (Right (EBin Sub (EBin Sub (int 1) (int 2)) (int 3))) (pe "1 - 2 - 3")
  , test ":: is right-associative" $
      assertEq (Right (EBin Cons (int 1) (EBin Cons (int 2) (EList [])))) (pe "1 :: 2 :: []")
  , test "application binds tighter than operators" $
      assertEq (Right (EBin Add (EApp (v "f") (int 1)) (EApp (v "g") (int 2)))) (pe "f 1 + g 2")
  , test "comparison below arithmetic, && below comparison" $
      assertEq (Right (EBin And (EBin Lt (v "a") (EBin Add (v "b") (int 1))) (EBin Eq (v "c") (v "d"))))
        (pe "a < b + 1 && c = d")
  , test "tuples and unit" $
      assertEq (Right (ETuple [int 1, ELit LUnit, ETuple [v "a", v "b"]])) (pe "(1, (), (a, b))")
  , test "list literal with trailing semicolon" $
      assertEq (Right (EList [int 1, int 2])) (pe "[1; 2;]")
  , test "fun with several parameters" $
      assertEq (Right (ELam "x" (ELam "y" (EBin Add (v "x") (v "y"))))) (pe "fun x y -> x + y")
  , test "let with parameters desugars to lambdas" $
      assertEq (Right (ELet "f" (ELam "x" (v "x")) (EApp (v "f") (int 1)))) (pe "let f x = x in f 1")
  , test "let rec" $
      assertEq (Right (ELetRec "f" (ELam "n" (EApp (v "f") (v "n"))) (v "f"))) (pe "let rec f n = f n in f")
  , test "tuple pattern in let becomes a match" $
      assertEq (Right (EMatch (v "p") [(PTuple [PVar "a", PVar "b"], v "a")])) (pe "let (a, b) = p in a")
  , test "match with leading bar and list patterns" $
      assertEq
        (Right (EMatch (v "xs") [(PNil, int 0), (PCons (PVar "x") (PCons PWild PNil), v "x")]))
        (pe "match xs with | [] -> 0 | x :: [_] -> x")
  , test "keyword expression as right operand" $
      assertEq (Right (EBin Add (int 1) (EIf (v "b") (int 2) (int 3)))) (pe "1 + if b then 2 else 3")
  , test "nested comments are skipped" $
      assertEq (Right (int 1)) (pe "(* outer (* inner *) still *) 1")
  , test "program with several declarations" $
      assertEq
        (Right 3)
        (length <$> pp "let x = 1\nlet rec f n = n ;; f x")
  , test "error: missing then" $ assertLeft "line 1, column 8: expected 'then'" (pe "if x 1 else 2")
  , test "error: unclosed paren" $ assertLeft "expected ')'" (pe "(1 + 2")
  , test "error: unterminated string" $ assertLeft "unterminated string" (pe "\"abc")
  , test "error: let rec needs a name" $ assertLeft "function name" (pe "let rec (a, b) = 1 in a")
  , test "error position on later line" $ assertLeft "line 2, column 6: expected an expression" (pe "let x =\n  1 +) in x")
  ]
