module InferSpec (inferTests) where

import Harness
import MiniML.Driver

ty :: String -> Either String String
ty = typeOf preludeSession

ty0 :: String -> Either String String
ty0 = typeOf baseSession

inferTests :: (String, [Test])
inferTests = group "inference"
  [ test "identity" $ assertEq (Right "'a -> 'a") (ty0 "fun x -> x")
  , test "const" $ assertEq (Right "'a -> 'b -> 'a") (ty0 "fun x y -> x")
  , test "compose" $
      assertEq (Right "('a -> 'b) -> ('c -> 'a) -> 'c -> 'b") (ty0 "fun f g x -> f (g x)")
  , test "S combinator" $
      assertEq (Right "('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c") (ty0 "fun x y z -> x z (y z)")
  , test "let-polymorphism" $
      assertEq (Right "int * bool") (ty0 "let id = fun x -> x in (id 1, id true)")
  , test "lambda-bound variables stay monomorphic" $
      assertLeft "expected of type int" (ty0 "(fun id -> (id 1, id true)) (fun x -> x)")
  , test "arithmetic" $ assertEq (Right "int -> int") (ty0 "fun n -> n * 2 + 1")
  , test "equality is polymorphic" $ assertEq (Right "'a -> 'a -> bool") (ty0 "fun a b -> a = b")
  , test "list operators" $ assertEq (Right "'a -> 'a list -> 'a list") (ty0 "fun x xs -> x :: xs @ []")
  , test "tuples in types" $ assertEq (Right "'a * 'b -> 'b * 'a") (ty0 "fun p -> (snd p, fst p)")
  , test "recursive function" $
      assertEq (Right "int -> int") (ty0 "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact")
  , test "recursion is generalized after the binding" $
      assertEq (Right "int list * bool list")
        (ty "let rec m f xs = match xs with [] -> [] | x :: t -> f x :: m f t in (m (fun x -> x + 1) [1], m not [true])")
  , test "pattern matching on tuples of lists" $
      assertEq (Right "'a list -> 'b list -> ('a * 'b) list")
        (ty0 "let rec z xs ys = match (xs, ys) with (x :: a, y :: b) -> (x, y) :: z a b | _ -> [] in z")
  , test "prelude map" $ assertEq (Right "('a -> 'b) -> 'a list -> 'b list") (ty "map")
  , test "prelude foldr" $ assertEq (Right "('a -> 'b -> 'b) -> 'a list -> 'b -> 'b") (ty "foldr")
  , test "prelude assoc" $ assertEq (Right "'a -> ('a * 'b) list -> 'b") (ty "assoc")
  , test "partial application" $ assertEq (Right "int list -> int list") (ty "map (fun x -> x * x)")
  , test "higher-rank types are rejected" $
      assertLeft "type error" (ty0 "fun f -> (f 1, f true)")
  , test "occurs check" $ assertLeft "infinite type 'a = 'a -> 'b" (ty0 "fun x -> x x")
  , test "occurs check through lists" $ assertLeft "infinite type" (ty0 "fun x -> x :: x")
  , test "mismatch points at the argument" $
      assertLeft "line 1, column 5: this expression has type bool but an expression was expected of type int"
        (ty0 "1 + true")
  , test "if branches must agree" $
      assertLeft "column 21: this expression has type string" (ty0 "if true then 1 else \"no\"")
  , test "condition must be bool" $ assertLeft "expected of type bool" (ty0 "if 1 then 2 else 3")
  , test "applying a non-function" $ assertLeft "not a function" (ty0 "1 2")
  , test "unbound variable" $ assertLeft "unbound variable 'nope'" (ty0 "nope + 1")
  , test "pattern type mismatch" $
      assertLeft "this pattern matches values of type 'a list but the matched value has type int"
        (ty0 "match 1 with [] -> 0 | _ -> 1")
  , test "duplicate pattern variable" $ assertLeft "bound several times" (ty0 "fun p -> match p with (x, x) -> x")
  , test "let rec must bind a function" $ assertLeft "must be a function" (ty0 "let rec x = x + 1 in x")
  , test "shadowing" $ assertEq (Right "bool") (ty0 "let x = 1 in let x = true in x")
  , test "list where an int is needed" $
      assertLeft "this expression has type int list but an expression was expected of type int"
        (ty0 "fun xs -> 1 + (1 :: xs)")
  ]
