{-# language LambdaCase #-}

module Theseus.AbstractSyntax
  ( Prog, Constr, TName, FName, Var, LName, ModuleName, Formals, Val
  , PVal(..)
  , Def(..)
  , Typ(..)
  , ITyp(..)
  , Func(..)
  , Clause(..)
  ) where

type Prog = [Def]

-- TODO: the case of various identifiers is not enforced
type Constr = String     -- constructor name (start with caps)
type TName = String      -- type name (start with caps)
type FName = String      -- function names (start with lowercase)
type Var = String        -- ordinary pattern variables (start with lowercase)
type LName = String      -- Label names
type ModuleName = String -- Module names are filenames without ".ths"
                         -- and with the first letter capitalized

data Def
  = DataTyp TName [(Constr,Typ)]
  | Iso FName Formals ITyp [(LName, Typ)] [Clause]
  | Eval Func PVal
  | Import ModuleName
  deriving (Eq, Ord, Show)

type Formals = [(FName, ITyp)]

data Typ
  = One
  | Zero
  | Times Typ Typ
  | Plus Typ Typ
  | TName TName
  | Neg Typ
  deriving (Eq, Ord, Show)

-- | Isomorphisms, `a = b`.
data ITyp = ITyp Typ Typ
  deriving (Eq, Ord, Show)

data PVal
  = Unit -- ^ unit
  | Pair PVal PVal -- ^ pairs
  | LeftE PVal -- ^ sum (left con)
  | RightE PVal -- ^ sum (right con)
  | Constr Constr PVal -- ^ pattern starting with constructor
  | Minus PVal -- ^ a negative value
  | Var Var -- ^ pattern variable (cannot show up in values)
  | App Func PVal -- ^ function call (cannot show up in values)
  deriving (Eq, Show)

ordSeq :: Ordering -> Ordering -> Ordering
ordSeq = \case
  EQ -> id
  x  -> const x

ctorNum :: PVal -> Int
ctorNum = \case
  Unit -> 0
  Pair{} -> 1
  LeftE{} -> 2
  RightE{} -> 3
  Constr{} -> 4
  Minus{} -> 5
  Var{} -> -1
  App{} -> -1

instance Ord PVal where
  compare p p' =
    (compare (ctorNum p) (ctorNum  p')) `ordSeq` (comp p p')
    where
      comp Unit Unit = EQ
      comp (Pair p1 p2) (Pair p1' p2') =
        (compare p1 p1') `ordSeq` (compare p2 p2')
      comp (LeftE p1) (LeftE p2) = compare p1 p2
      comp (RightE p1) (RightE p2) = compare p1 p2
      comp (Constr c1 p1) (Constr c2 p2) =
        (compare c1 c2) `ordSeq` (compare p1 p2)
      comp (Minus p1) (Minus p2) = compare p1 p2
      comp (Var _) (Var _) = EQ
      comp (Var _) (App _ _) = EQ
      comp (App _ _) (Var _) = EQ
      comp (App _ _) (App _ _) = EQ
      comp p1 p2 = compare (ctorNum p1) (ctorNum p2)

-- we intend this to mean just the value fragement of PVal, but there
-- is no nice way of saying that right now.
type Val = PVal

data Func = Func FName [(String, Maybe Func)]
  deriving (Show, Eq, Ord)

data Clause = Clause (Maybe LName, PVal) (Maybe LName, PVal)
  deriving (Show, Eq, Ord)

