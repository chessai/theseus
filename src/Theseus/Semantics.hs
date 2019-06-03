{-# language LambdaCase #-}

module Theseus.Semantics
  ( FEnv(..)
  , Cur(..)
  , evalProg
  ) where

import Theseus.AbstractSyntax
import Theseus.Coverage

-- the value environment collects declared constants and isomorphisms
type VEnv = [(Var, Val)]
type Clauses = [Clause]
newtype FEnv = FEnv [(FName, (Clauses, FEnv))]

data Dir = L | R
  deriving Show

opp :: Dir -> Dir
opp = \case { L -> R; R -> L };

projPattern :: Clause -> Dir -> (Maybe LName, PVal)
projPattern (Clause lhs _ ) L = lhs
projPattern (Clause _ rhs) R = rhs

projPattern2 :: Clause -> Dir -> ((Maybe LName, PVal), (Maybe LName, PVal))
projPattern2 cl dir = (projPattern cl dir, projPattern cl (opp dir))

data Cur = Cur
  { curDir :: Dir
  , curLabel :: Maybe LName
  , curVal :: Val
  } deriving Show

-- Deconstruct a value using a pattern. This builds an environment
-- binding the variables introduced by the pattern and their values.
deconst :: FEnv -> PVal -> Val -> VEnv -> Maybe VEnv
deconst _fenv Unit Unit env = Just env
deconst fenv (LeftE p) (LeftE v) env = deconst fenv p v env
deconst fenv (RightE p) (RightE v) env = deconst fenv p v env
deconst fenv (Pair p1 p2) (Pair v1 v2) env = f (deconst fenv p1 v1 env)
    where
      f Nothing = Nothing
      f (Just env') = deconst fenv p2 v2 env'
deconst fenv (Constr c p) (Constr c1 v) env | c == c1 = deconst fenv p v env
deconst _fenv (Var x) v env = Just ((x, v):env)
deconst fenv (App func p) v env =
    let (Cur {curVal=v'}) = evalFunc func fenv (Cur {curVal=v, curDir=R, curLabel=Nothing}) in
    deconst fenv p v' env
deconst _ _ _ _ = Nothing

-- Reconstruct a value from a pattern and an environment that supplies
-- the values of the variables in the pattern.
reconst :: FEnv -> VEnv -> PVal -> Maybe (VEnv, PVal)
reconst _fenv env Unit = return (env, Unit)
reconst fenv env (LeftE p) =
    do (env1, v) <- reconst fenv env p
       return (env1, LeftE v)
reconst fenv env (RightE p) =
    do (env1, v) <- reconst fenv env p
       return (env1, RightE v)
reconst fenv env (Pair p1 p2) =
    do (env1, v1) <- reconst fenv env p1
       (env2, v2) <- reconst fenv env1 p2
       return (env2, (Pair v1 v2))
reconst fenv env (Constr c p) =
    do (env1, v) <- reconst fenv env p
       return (env1, (Constr c v))
reconst fenv env (App func p) =
    do (env1, v) <- reconst fenv env p
       let (Cur {curVal=v'}) = evalFunc func fenv (Cur {curVal=v, curDir=L, curLabel=Nothing})
       return (env1, v')
reconst _fenv env (Var x) = extract env x
    where
      extract [] _ = Nothing
      extract ((x1, v1):env') x'
        | x1 == x' = return (env', v1)
        | otherwise = do
            (env1, v) <- extract env' x
            return ((x1, v1):env1, v)

evalClauses :: [Clause] -> FEnv -> Cur  -> Cur
evalClauses clauses fenv cur = apply clauses
    where
      apply [] = error $ "none of the patterns matched " ++ (show cur)
      apply (cl:cls) =
          let ((lopt1, p1), (lopt2, p2)) = projPattern2 cl (curDir cur) in
          case (lopt1 == (curLabel cur), deconst fenv p1 (curVal cur) []) of
            (True, Just env) ->
                let v = ret cl (reconst fenv env p2) in
                let cur1 = cur { curVal = v, curLabel = lopt2 } in
                maybeIter cur1
            _ -> apply cls
      ret _cl (Just ([], v)) = v
      ret cl Nothing = error $ "reconstruction failed " ++ (show cl) ++ " value " ++ (show cur)
      ret cl (Just (env, _)) = error $ "non-empty env returned : " ++ (show env) ++ " " ++ show cl ++ " " ++ show cur
      maybeIter cur'@(Cur { curLabel = Nothing } ) = cur'
      maybeIter cur' = evalClauses clauses fenv cur'

evalFuncArgs :: Func -> FEnv -> (Clauses, FEnv)
evalFuncArgs (Func fname args) fenv0 = evalArgs args fenv0
    where
      evalArgs [] (FEnv env) = lookup_exn fname env
      evalArgs ((name, Nothing):args') fenv' = evalArgs ((name, Just (Func name [])):args') fenv'
      evalArgs ((name, Just func):args') fenv =
          let closure = evalFuncArgs func fenv in
          let (clauses, FEnv fenv') = evalArgs args' fenv in
          (clauses, FEnv ((name, closure):fenv'))

evalFunc :: Func -> FEnv -> Cur -> Cur
evalFunc func fenv cur = -- for now the args are ignored
    -- trace ("eval " ++ (show func) ++ (show cur)) $
    let (clauses, fenv') =  evalFuncArgs func fenv in
    let res = evalClauses clauses fenv' cur in
    -- seq res (trace ("ret from " ++ (show func) ++ show res) res)
    res

-- remove all the IO from here.
evalProg :: [Def] -> FEnv -> Bool -> IO (FEnv, [(Func, Val, Cur)])
evalProg [] fenv do_eval =
  return (fenv, [])
evalProg (DataTyp _ _ : defs) env do_eval =
  evalProg defs env do_eval
evalProg (Iso name params ityp labels clauses : defs) (FEnv env) do_eval =
  evalProg defs (FEnv ((name, (clauses, FEnv env)):env)) do_eval
evalProg ((Eval func val) : defs) env do_eval =
  do (newfenv, rest) <- evalProg defs env do_eval
     if do_eval
     then
       let cur = evalFunc func env (Cur {curLabel = Nothing, curDir = L, curVal = val}) in
       return $ (newfenv, (func, val, cur):rest)
     else
       return (newfenv, rest)
evalProg ((Import (f:fs)) : defs) (FEnv fenv) do_eval =
  error "Unexpected import"

