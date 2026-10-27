-- | Randomised properties:
--
-- * type soundness: a random expression that type-checks never gets
--   stuck at run time, and its value inhabits the inferred type;
-- * parser round trip: printing a random expression and re-parsing it
--   yields the same tree.
module PropertySpec (propertyTests) where

import Data.Bits (shiftR, xor)
import Data.Word (Word64)
import Harness
import MiniML.Builtins (builtinTypes, builtinValues)
import MiniML.Eval (evalExpr)
import MiniML.Infer (inferExpr)
import MiniML.Parser (parseExpr, renderParseError)
import MiniML.Syntax
import MiniML.Types
import MiniML.Value

----------------------------------------------------------------------
-- SplitMix64 and a small generator monad

newtype Gen a = Gen { runGen :: Word64 -> (a, Word64) }

instance Functor Gen where
  fmap f (Gen g) = Gen $ \s -> let (a, s') = g s in (f a, s')

instance Applicative Gen where
  pure a = Gen $ \s -> (a, s)
  gf <*> ga = do { f <- gf; f <$> ga }

instance Monad Gen where
  Gen g >>= k = Gen $ \s -> let (a, s') = g s in runGen (k a) s'

nextWord :: Gen Word64
nextWord = Gen $ \s ->
  let s' = s + 0x9e3779b97f4a7c15
      z1 = (s' `xor` (s' `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
  in (z2 `xor` (z2 `shiftR` 31), s')

choose :: Int -> Gen Int
choose n = (\w -> fromIntegral (w `mod` fromIntegral n)) <$> nextWord

oneOf :: [Gen a] -> Gen a
oneOf gs = choose (length gs) >>= (gs !!)

elements :: [a] -> Gen a
elements xs = (xs !!) <$> choose (length xs)

----------------------------------------------------------------------
-- Expression generator (biased toward well-typed programs)

binders :: [Name]
binders = ["a", "b", "c", "f", "g"]

prims :: [Name]
prims = ["not", "fst", "snd", "string_of_int", "string_length"]

genLit :: Gen Lit
genLit = oneOf
  [ LInt . subtract 3 . fromIntegral <$> choose 8
  , LBool . (== 0) <$> choose 2
  , LStr <$> elements ["", "x", "hi"]
  , pure LUnit
  ]

genExpr :: [Name] -> Int -> Gen Expr
genExpr scope depth
  | depth <= 0 = leaf
  | otherwise = do
      k <- choose 14
      case k of
        0 -> leaf
        1 -> leaf
        2 -> do { x <- elements binders; ELam x <$> sub (x : scope) }
        3 -> EApp <$> sub scope <*> sub scope
        4 -> EApp <$> (EVar <$> elements (prims ++ scope)) <*> sub scope
        5 -> do { x <- elements binders; ELet x <$> sub scope <*> sub (x : scope) }
        6 -> EIf <$> sub scope <*> sub scope <*> sub scope
        7 -> do { o <- elements [minBound .. maxBound]; EBin o <$> sub scope <*> sub scope }
        8 -> do { o <- elements [Add, Sub, Mul, Lt, Eq]; EBin o <$> sub scope <*> leaf }
        9 -> (\a b -> ETuple [a, b]) <$> sub scope <*> sub scope
        10 -> do { n <- choose 3; EList <$> mapM (const (sub scope)) [1 .. n] }
        11 -> EBin Cons <$> sub scope <*> sub scope
        _ -> do
          s <- sub scope
          n <- (+ 1) <$> choose 2
          arms <- mapM (const genArm) [1 .. n]
          pure (EMatch s arms)
  where
    sub sc = genExpr sc (depth - 1)
    leaf = oneOf $ [ELit <$> genLit, pure (EList [])]
      ++ [EVar <$> elements scope | not (null scope)]
      ++ [EVar <$> elements prims]
    genArm = do
      p <- genPat 2
      body <- genExpr (patVars p ++ scope) (depth - 1)
      pure (p, body)

genPat :: Int -> Gen Pat
genPat depth = do
  k <- choose (if depth <= 0 then 4 else 7)
  case k of
    0 -> pure PWild
    1 -> PVar <$> elements binders
    2 -> PLit <$> genLit
    3 -> pure PNil
    4 -> PCons <$> genPat (depth - 1) <*> genPat (depth - 1)
    5 -> (\a b -> PTuple [a, b]) <$> genPat (depth - 1) <*> genPat (depth - 1)
    _ -> PVar <$> elements binders

sample :: Int -> Gen a -> a
sample seed g = fst (runGen g (fromIntegral seed * 0x2545F4914F6CDD1D + 1))

----------------------------------------------------------------------
-- Properties

-- | Does a value inhabit a type? Type variables accept anything.
inhabits :: Type -> Value -> Bool
inhabits t v = case (t, v) of
  (TVar _, _) -> True
  (TCon "int" [], VInt _) -> True
  (TCon "bool" [], VBool _) -> True
  (TCon "string" [], VStr _) -> True
  (TCon "unit" [], VUnit) -> True
  (TCon "list" [e], VList xs) -> all (inhabits e) xs
  (TCon "*" ts, VTuple vs) -> length ts == length vs && and (zipWith inhabits ts vs)
  (TCon "->" _, VClosure {}) -> True
  (TCon "->" _, VPrim {}) -> True
  _ -> False

data Outcome = Untyped | Ok | Failed String

soundnessOne :: Int -> Outcome
soundnessOne seed =
  let e = sample seed (genExpr [] 5)
  in case inferExpr builtinTypes e of
       Left _ -> Untyped
       Right (Forall _ t) -> case evalExpr 100000 builtinValues e of
         Left (Stuck msg) -> Failed ("seed " ++ show seed ++ " got stuck (" ++ msg ++ "): " ++ prettyExpr e)
         Left _ -> Ok
         Right v
           | inhabits t v -> Ok
           | otherwise -> Failed ("seed " ++ show seed ++ ": value " ++ showValue v ++ " does not have type " ++ prettyType t ++ " in " ++ prettyExpr e)

soundness :: Int -> Either String ()
soundness n = go 0 [0 .. n - 1]
  where
    go typed [] = assertBool ("only " ++ show typed ++ " of " ++ show n ++ " random programs were well-typed") (typed * 20 >= n)
    go typed (s : ss) = case soundnessOne s of
      Untyped -> go typed ss
      Ok -> go (typed + 1 :: Int) ss
      Failed msg -> Left msg

roundTrip :: Int -> Either String ()
roundTrip n = mapM_ one [0 .. n - 1]
  where
    one seed =
      let e = sample (seed + 1000003) (genExpr [] 4)
          printed = prettyExpr e
      in case parseExpr printed of
           Left err -> Left ("seed " ++ show seed ++ ": " ++ renderParseError err ++ " in " ++ printed)
           Right e'
             | stripPos e' == e -> Right ()
             | otherwise -> Left ("seed " ++ show seed ++ ": " ++ printed ++ " re-parsed as " ++ prettyExpr e')

propertyTests :: (String, [Test])
propertyTests = group "properties"
  [ test "well-typed random programs do not go wrong (4000 seeds)" (soundness 4000)
  , test "print/parse round trip (2000 seeds)" (roundTrip 2000)
  ]
