module EvalSpec (evalTests) where

import Harness
import MiniML.Driver

ev :: String -> Either String String
ev src = snd <$> evalString src

-- | Run a whole program and return the printed lines.
prog :: String -> Either String [String]
prog src = snd <$> runSource Run preludeSession src

evalTests :: (String, [Test])
evalTests = group "evaluation"
  [ test "arithmetic truncates like OCaml" $ assertEq (Right "(-3, -1, 3)") (ev "(-7 / 2, -7 % 2, 7 / 2)")
  , test "big integers" $ assertEq (Right "1267650600228229401496703205376") (ev "iterate 100 (fun x -> x * 2) 1")
  , test "string concatenation" $ assertEq (Right "\"ab-12\"") (ev "\"a\" ^ \"b\" ^ \"-\" ^ string_of_int 12")
  , test "closures capture their environment" $
      assertEq (Right "15") (ev "let add n = fun x -> x + n in let add5 = add 5 in add5 10")
  , test "recursion" $
      assertEq (Right "3628800") (ev "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 10")
  , test "mutual-style recursion through a tuple-free accumulator" $
      assertEq (Right "true") (ev "let rec even n = if n = 0 then true else if n = 1 then false else even (n - 2) in even 100")
  , test "short-circuit && and ||" $
      assertEq (Right "(false, true)") (ev "(false && hd [] = 1, true || hd [] = 1)")
  , test "structural equality on nested data" $
      assertEq (Right "(true, false)") (ev "([(1, \"a\")] = [(1, \"a\")], [1; 2] = [1; 3])")
  , test "lexicographic comparison" $
      assertEq (Right "(true, true, true)") (ev "([1; 2] < [1; 3], (2, 0) > (1, 9), \"abc\" < \"abd\")")
  , test "pattern matching with literals and nesting" $
      assertEq (Right "[\"zero\"; \"one\"; \"many\"]")
        (ev "map (fun n -> match n with 0 -> \"zero\" | 1 -> \"one\" | _ -> \"many\") [0; 1; 5]")
  , test "tuple parameters in fun" $ assertEq (Right "[3; 7]") (ev "map (fun (a, b) -> a + b) [(1, 2); (3, 4)]")
  , test "prelude: filter, range, sum" $
      assertEq (Right "2550") (ev "sum (filter (fun x -> x % 2 = 0) (range 0 101))")
  , test "prelude: rev, zip, assoc" $
      assertEq (Right "(\"c\", [3; 2; 1])") (ev "(assoc 3 (zip [1; 2; 3] [\"a\"; \"b\"; \"c\"]), rev [1; 2; 3])")
  , test "prelude: string_concat and concat_map" $
      assertEq (Right "\"1,1,2,2\"") (ev "string_concat \",\" (map string_of_int (concat_map (fun x -> [x; x]) [1; 2]))")
  , test "deep recursion" $ assertEq (Right "100000") (ev "length (range 0 100000)")
  , test "partial application of primitives" $ assertEq (Right "[1; 2]") (ev "map fst [(1, true); (2, false)]")
  , test "program output" $
      assertEq (Right ["val sq : int -> int = <fun>", "val xs : int list = [1; 4; 9]", "- : int = 14"])
        (prog "let sq x = x * x\nlet xs = map sq [1; 2; 3]\n;; sum xs")
  , test "top-level tuple destructuring" $
      assertEq (Right ["val a : int = 1", "val b : string = \"x\""]) (prog "let (a, b) = (1, \"x\")")
  , test "polymorphic top-level values print schemes" $
      assertEq (Right ["val pair : 'a -> 'b -> 'a * 'b = <fun>"]) (prog "let pair a b = (a, b)")
  , test "failwith" $ assertLeft "runtime error: exception: boom" (ev "1 + failwith \"boom\"")
  , test "division by zero" $ assertLeft "division by zero" (ev "1 / (2 - 2)")
  , test "match failure" $ assertLeft "match failure: no case matches [1]" (ev "match [1] with [] -> 0")
  , test "comparing functions fails at runtime" $ assertLeft "functional value" (ev "(fun x -> x) = (fun y -> y)")
  , test "hd of empty list" $ assertLeft "hd: empty list" (ev "hd []")
  , test "int_of_string" $ assertEq (Right "-42") (ev "int_of_string \"-42\"")
  , test "non-termination hits the step limit" $
      assertLeft "step limit" (ev "let rec loop x = loop x in loop 0")
  ]
