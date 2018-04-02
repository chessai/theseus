module Theseus.AbstractSyntax where

debug :: Bool
debug = False

type Prog = [Def]

-- TODO: the case of various identifiers is not enforced
type Constr = String     -- constructor name (start with caps)
type TName = String      -- type name (start with caps)
type FName = String      -- function names (start with lowercase)
type Var = String        -- ordinary pattern variables (start with lowercase)
type LName = String      -- Label names
type ModuleName = String -- Module names are filenames without ".ths"
                         -- and with the first letter capitalized

data Def = DataTyp TName [(Constr,Typ)] 
           -- e.g.: data Tree = Leaf Nat | Node (Tree * Tree) 
         | Iso FName Formals ITyp [(LName, Typ)] [Clause]
         | Eval Func PVal
         | Import ModuleName
           deriving (Eq, Ord)
type Formals = [(FName, ITyp)]

data Typ = One 
         | Zero
         | Times Typ Typ
         | Plus Typ Typ -- plus is not essential, but it is convenient
                        -- to have so that one does not have to define
                        -- a new type for every sum. 
         | TName TName  -- for user defined types 
         | Neg Typ
           deriving (Eq, Ord)

data ITyp = ITyp Typ Typ -- t <-> t
            deriving (Eq, Ord)
                     
data PVal = Unit               -- unit
          | Pair PVal PVal     -- pairs
          | LeftE PVal         -- sum
          | RightE PVal        -- sum
          | Constr Constr PVal -- a pattern starting with a constructor
          | Minus PVal         -- a negative value
          -- all the subsequent cases cannot show up in values. 
          | Var Var            -- pattern variable  
          | App Func PVal      -- function call
            deriving Eq

ordSeq :: Ordering -> Ordering -> Ordering
ordSeq EQ x = x
ordSeq x _ = x

ctorNum :: PVal -> Int
ctorNum Unit = 0
ctorNum (Pair _ _) = 1
ctorNum (LeftE _) = 2
ctorNum (RightE _) = 3
ctorNum (Constr _ _) = 4
ctorNum (Minus _) = 5
ctorNum (Var _) = -1
ctorNum (App _ _) = -1

instance Ord PVal where
  compare p1 p2 =
    (compare (ctorNum p1) (ctorNum  p2)) `ordSeq` (comp p1 p2)
    where 
      comp Unit Unit = EQ
      comp (Pair a b) (Pair x y) =
        (compare a x) `ordSeq` (compare b y)
      comp (LeftE a) (LeftE b) = compare a b
      comp (RightE a) (RightE b) = compare a b
      comp (Constr a b) (Constr x y) =
        (compare a x) `ordSeq` (compare b y)
      comp (Minus a) (Minus b) = compare a b
      comp (Var _) (Var _) = EQ
      comp (Var _) (App _ _) = EQ
      comp (App _ _) (Var _) = EQ
      comp (App _ _) (App _ _) = EQ

-- CR rjames: In the case of App, we want to ensure that the PVal
-- covers the type.

-- we itend this to mean just the value fragement of PVal, but there
-- is no nice way of saying that right now.
type Val = PVal

data Func = Func FName [(String, Maybe Func)]
          deriving (Show, Eq, Ord)

data Clause = Clause (Maybe LName, PVal) (Maybe LName, PVal)
              deriving (Eq, Ord)
