-- | Types, type schemes, substitutions and type pretty-printing.
module MiniML.Types
  ( TVar
  , Type (..)
  , Scheme (..)
  , Subst
  , tInt, tBool, tString, tUnit, tList, tTuple, (~>)
  , Types (..)
  , emptySubst
  , compose
  , prettyType
  , prettyTypes
  , prettyScheme
  , normalizeScheme
  ) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type TVar = Int

-- | Monotypes. Constructors: @int@, @bool@, @string@, @unit@,
-- @list@ (one argument), @->@ (two) and @*@ (tuples, two or more).
data Type
  = TVar TVar
  | TCon String [Type]
  deriving (Eq, Ord, Show)

-- | A polytype: @forall vs. t@.
data Scheme = Forall [TVar] Type
  deriving (Eq, Show)

type Subst = Map.Map TVar Type

tInt, tBool, tString, tUnit :: Type
tInt = TCon "int" []
tBool = TCon "bool" []
tString = TCon "string" []
tUnit = TCon "unit" []

tList :: Type -> Type
tList t = TCon "list" [t]

tTuple :: [Type] -> Type
tTuple = TCon "*"

infixr 5 ~>
(~>) :: Type -> Type -> Type
a ~> b = TCon "->" [a, b]

class Types a where
  ftv :: a -> Set.Set TVar
  apply :: Subst -> a -> a

instance Types Type where
  ftv (TVar v) = Set.singleton v
  ftv (TCon _ ts) = Set.unions (map ftv ts)
  apply s t@(TVar v) = Map.findWithDefault t v s
  apply s (TCon c ts) = TCon c (map (apply s) ts)

instance Types Scheme where
  ftv (Forall vs t) = ftv t `Set.difference` Set.fromList vs
  apply s (Forall vs t) = Forall vs (apply (foldr Map.delete s vs) t)

instance Types a => Types [a] where
  ftv = Set.unions . map ftv
  apply s = map (apply s)

instance Types a => Types (Map.Map k a) where
  ftv = ftv . Map.elems
  apply s = Map.map (apply s)

emptySubst :: Subst
emptySubst = Map.empty

-- | @compose s2 s1@ applies @s1@ first, then @s2@.
compose :: Subst -> Subst -> Subst
compose s2 s1 = Map.map (apply s2) s1 `Map.union` s2

-- | Type variables in order of first appearance.
varsInOrder :: Type -> [TVar]
varsInOrder t = nub (go t)
  where
    go (TVar v) = [v]
    go (TCon _ ts) = concatMap go ts

varName :: Int -> String
varName i
  | i < 26 = ['\'', toEnum (fromEnum 'a' + i)]
  | otherwise = '\'' : toEnum (fromEnum 'a' + i `mod` 26) : show (i `div` 26)

-- | Pretty-print several types with a shared variable naming, so that the
-- same variable gets the same name in each.
prettyTypes :: [Type] -> [String]
prettyTypes ts = map (render names 0) ts
  where
    names = Map.fromList (zip (varsInOrder (TCon "" ts)) (map varName [0 ..]))

prettyType :: Type -> String
prettyType t = head (prettyTypes [t])

prettyScheme :: Scheme -> String
prettyScheme (Forall _ t) = prettyType t

-- | Rename the quantified variables of a scheme to 0, 1, 2 ... in order
-- of appearance; handy for comparing schemes structurally.
normalizeScheme :: Scheme -> Scheme
normalizeScheme (Forall _ t) = Forall (map snd pairs) (apply s t)
  where
    pairs = zip (varsInOrder t) [0 ..]
    s = Map.fromList [(v, TVar n) | (v, n) <- pairs]

-- Precedence: 0 = arrow, 1 = tuple, 2 = type application.
render :: Map.Map TVar String -> Int -> Type -> String
render names = go
  where
    go _ (TVar v) = Map.findWithDefault ("'_" ++ show v) v names
    go p (TCon "->" [a, b]) = paren (p > 0) (go 1 a ++ " -> " ++ go 0 b)
    go p (TCon "*" ts) = paren (p > 1) (joinStar (map (go 2) ts))
    go _ (TCon c []) = c
    go _ (TCon c [a]) = go 2 a ++ " " ++ c
    go _ (TCon c as) = "(" ++ joinComma (map (go 0) as) ++ ") " ++ c
    paren True s = "(" ++ s ++ ")"
    paren False s = s
    joinStar = foldr1 (\a b -> a ++ " * " ++ b)
    joinComma = foldr1 (\a b -> a ++ ", " ++ b)
