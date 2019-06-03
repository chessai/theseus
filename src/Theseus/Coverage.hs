{-# language LambdaCase #-}

module Theseus.Coverage
  ( lookup_exn
  , reportErrors
  , tProg
  ) where

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Data.Char as Char
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List as List
import Debug.Trace
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token
import Text.PrettyPrint.HughesPJ hiding (char,comma,parens,integer,space)
import Text.Printf
import qualified Control.Monad.State as ST
import qualified Text.PrettyPrint.HughesPJ as PP (char,comma,parens,integer,space)

import Theseus.AbstractSyntax
import Theseus.Debug

type TypEnv = [(TName, [(Constr, Typ)])]
type CurTyp = (Typ, TypEnv)

lookup_exn v as = f (lookup v as)
    where
      f (Just v) = v
      f Nothing = error ("can't find " ++ (show v))

matchesAny :: PVal -> Bool
matchesAny = \case
  Var{} -> True
  App{} -> True
  _ -> False

-- expand out patterns of the type till the granularity of the
-- specified pattern is achieved.
expand :: PVal -> CurTyp -> [PVal]
expand Unit (One, _) = [Unit]
expand x (One, _) | matchesAny x = [Unit] -- just for better errors
expand x (_, _) | matchesAny x = [x]
expand (Pair p1 p2) (Times t1 t2, env) =
    [Pair v1 v2 | v1 <- expand p1 (t1, env),
                  v2 <- expand p2 (t2, env)]
expand (LeftE p) (Plus t1 t2, env) =
    [LeftE v | v <- expand p (t1, env)] ++ [RightE (Var "_")]
expand (RightE p) (Plus t1 t2, env) =
    [LeftE (Var "_")] ++ [RightE v | v <- expand p (t2, env)]
expand (Constr c p) (TName t, env) =
    concatMap
    (\(c', t') -> if c == c'
                  then [Constr c' v | v <- expand p (t', env)]
                  else if t' == One
                       then [Constr c' Unit]
                       else [Constr c' (Var "_")])
    (lookup_exn t env)
expand _ _ = [] -- fails only if the pattern doesn't match the type.

-- check if two patterns are the same
-- if they will satisfy exactly the same values, then they are returned.
-- if one is more general than the other, then it is split up into
--   more specific subpatterns
-- if they can't be reconciled then no patterns are returned
reconcile :: PVal -> PVal -> CurTyp -> ([PVal], [PVal])
reconcile x y typ | matchesAny x && matchesAny y = ([x], [y])
reconcile x p typ | matchesAny x = (expand p typ, [p])
reconcile p x typ | matchesAny x = ([p], expand p typ)
reconcile (Unit) (Unit) (One, _) = ([Unit], [Unit])
reconcile (Pair p1 p2) (Pair p1' p2') (Times t1 t2, env) =
    ([Pair v1 v2 | v1 <- r1, v2 <- r2],
     [Pair v1 v2 | v1 <- r1', v2 <- r2'])
    where
      (r1, r1') = reconcile p1 p1' (t1, env)
      (r2, r2') = reconcile p2 p2' (t2, env)
reconcile (LeftE p1) (LeftE p2) (Plus t1 t2, env) =
    ([LeftE v | v <- r1], [LeftE v | v <- r2])
    where (r1, r2) = reconcile p1 p2 (t1, env)
reconcile (RightE p1) (RightE p2) (Plus t1 t2, env) =
    ([RightE v | v <- r1], [RightE v | v <- r2])
    where (r1, r2) = reconcile p1 p2 (t2, env)
reconcile (Constr c1 p1) (Constr c2 p2) (TName t, env) | c1 == c2 =
    let cs = lookup_exn t env in
    let t1 = lookup_exn c1 cs in
    let (r1, r2) = reconcile p1 p2 (t1, env) in
    ([Constr c1 v | v <- r1],
     [Constr c2 v | v <- r2])
reconcile _ _ _ = ([], [])

-- errors
data CoverageError
  = NoMatches String PVal
  | MultipleMatches String PVal

reportErrors :: [CoverageError] -> IO ()
reportErrors [] = return ()
reportErrors (MultipleMatches msg p:ps) =
    do putStr $ "Error: " ++ msg ++ ": Multiple patterns match values of the form : " ++ (show p) ++ "\n"
       reportErrors ps
reportErrors (NoMatches msg p:ps) =
    do putStr $ "Error: " ++ msg ++ ": No patterns match values of the form : " ++ (show p) ++ "\n"
       reportErrors ps

-- check if two lists of patterns exactly cover each other
covers :: String -> [PVal] -> [PVal] -> CurTyp -> [CoverageError]
covers msg [] [] _ = []
covers msg [] ps@(p:_) _ = map (MultipleMatches msg) ps
covers msg ps@(p:_) [] _ = map (NoMatches msg) ps
covers msg (a:as) (b:bs) ct = trace_next (reconcile a b ct)
    where
      trace_covers as bs ct =
          if debug
          then trace ("| cover (" ++ show as ++ ") (" ++ show bs ++ ")") (covers msg as bs ct)
          else covers msg as bs ct
      trace_next vs =
          if debug
          then trace ("| next  " ++ (show vs)) next vs
          else next vs
      next ([a], [b]) = trace_covers as bs ct
      next (a1:as1, b1:bs1) = trace_covers (a1:as1 ++ as) (b1:bs1 ++ bs) ct
      next (_, _) =
          if a < b
          then (NoMatches msg a):(covers msg as (b:bs) ct)
          else (MultipleMatches msg b):(covers msg (a:as) bs ct)

-- check if a list of patterns are axhaustive for a type.
exhaustive :: String -> [PVal] -> CurTyp -> [CoverageError]
exhaustive context ps ct =
    let ps' = List.sort ps in
    let res = covers context [Var "_"] ps' ct in
    if debug
    then trace ("| Checking " ++ context ++ " : " ++ (show ps')) res
    else res

extend :: [(Maybe LName, [PVal])] -> (Maybe LName, PVal) -> [(Maybe LName, [PVal])]
extend [] (x, val) = [(x, [val])]
extend ((x,ls):env) (x1, val1)
  | x == x1 = (x, val1:ls):env
  | otherwise = (x, ls):(extend env (x1, val1))

-- this only checks coverage of LHS and RHS clauses of each ISO
-- at a minimum we need to type check each clause also.
tProg :: [Def] -> [CoverageError]
tProg defs = loop defs emptyEnv
    where
      emptyEnv = []
      loop [] _ = []
      loop ((DataTyp name args):defs) env = loop defs ((name, List.sort args):env)
      loop ((Import modname):defs) env = loop defs env -- ignoring imports for now
      loop ((Eval name val):defs) env = loop defs env -- ignoring the main
      loop ((Iso name params ityp labels clauses):defs) env =
          let types = packTypes ityp labels in
          (process name clauses types env (\(Clause lhs _) -> lhs) (\(ITyp t1 t2)->t1) "LHS")
          ++ (process name clauses types env (\(Clause _ rhs) -> rhs) (\(ITyp t1 t2)->t2) "RHS")
          ++ (loop defs env)
      classifyClauses clauses f = foldl (\env clause -> extend env (f clause)) [] clauses
      nameOf dir name (Just label) = dir ++ " of " ++ name ++ " $ " ++ label
      nameOf dir name (Nothing) = dir ++ " of " ++ name
      packTypes ityp labels =
          (Nothing, ityp):(map (\(lbl, typ) -> (Just lbl, ITyp typ typ)) labels)
      process name clauses types env fc ft dir =
          concatMap (\(lopt, cls) ->
                     let str = nameOf dir name lopt in
                     let typ = ft $ lookup_exn lopt types in
                     exhaustive str cls (typ, env))
                (classifyClauses clauses fc)
