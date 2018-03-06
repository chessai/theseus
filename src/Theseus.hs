module Theseus where

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import qualified Control.Monad.State as ST

import Data.List as List
import Data.Char as Char
import Debug.Trace

import Text.PrettyPrint.HughesPJ 
  hiding (char,comma,parens,integer,space)
import Text.Printf
import qualified Text.PrettyPrint.HughesPJ 
  as PP (char,comma,parens,integer,space)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

-----------------------------------------------------------------------
debug = False
-- debug = True

-----------------------------------------------------------------------
-- Abstract syntax

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

data ITyp = ITyp Typ Typ -- t = t
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
  

-- CR rjames: In the case of App, we want to ensure that the PVal
-- covers the type.

-- we itend this to mean just the value fragement of PVal, but there
-- is no nice way of saying that right now.
type Val = PVal

data Func = Func FName [(String, Maybe Func)]
          deriving (Show, Eq, Ord)

data Clause = Clause (Maybe LName, PVal) (Maybe LName, PVal)
              deriving (Show, Eq, Ord)

------------------------------------------------------------------------
-- Pretty printing

ppProg :: Prog -> Doc
ppProg []     = PP.space
ppProg (d:ds) = PP.space $+$ ppDef d $+$ ppProg ds

ppDef :: Def -> Doc
ppDef (DataTyp t alts) = 
  hsep [text "data", text t, equals, ppAlts alts]
ppDef (Iso name args ityp labels clauses) = 
    vcat [hsep [text "iso", text name, text ":", ppFArgs args, ppITyp ityp],
          vcat (map ppClause clauses),
          ppLabels labels]
ppDef (Eval func val) = 
    hsep [text "eval", ppFunc func, ppVal val]
ppDef (Import modname) = 
    hsep [text "import", text modname]
    

ppAlts :: [(Constr,Typ)] -> Doc
ppAlts [] = PP.space
ppAlts [(constr,One)] = 
  hsep [text constr] -- drop unit args of ctors
ppAlts [(constr,typ)] = 
  hsep [text constr, ppTyp typ]
ppAlts ((constr,One):alts) = 
  hsep [text constr, text "|", ppAlts alts]
ppAlts ((constr,typ):alts) = 
  hsep [text constr, ppTyp typ, text "|", ppAlts alts]

ppFArgs :: [(FName, ITyp)] -> Doc
ppFArgs [] = empty
ppFArgs ((fname, ityp):args) = 
    hsep [ hcat [text fname, text ":", text "(", ppITyp ityp, text ")"], 
           text "->", ppFArgs args]

ppTyp :: Typ -> Doc
ppTyp One = text "1"
ppTyp Zero = text "0"
ppTyp (TName t) = text t
ppTyp (Neg t) = hcat [text "-", ppTyp t]
ppTyp (Times t1 t2) = hsep [ppTypParen t1, text "*", ppTypParen t2]
ppTyp (Plus t1 t2)  = hsep [ppTypParen t1, text "+", ppTypParen t2]

ppITyp :: ITyp -> Doc
ppITyp (ITyp b1 b2) = hsep [ppTyp b1, text "=", ppTyp b2]


ppTypParen :: Typ -> Doc
ppTypParen t = 
  case t of 
    Times _ _ -> PP.parens (ppTyp t)
    Plus _ _ -> PP.parens (ppTyp t)
    _         -> ppTyp t

ppLabels :: [(LName, Typ)] -> Doc
ppLabels [] = empty
ppLabels ((label, typ):labels) = 
    vcat [ hsep [text "where", text label, text ":", ppTyp typ],
           ppLabels labels ]

ppVal :: PVal -> Doc 
ppVal Unit = text "()"
ppVal (LeftE v)    = hsep [text "inL", ppValParen v]
ppVal (RightE v)   = hsep [text "inR", ppValParen v]
ppVal (Pair v1 v2) = sep [hcat [ppVal v1, PP.comma],
                           nest 2 $ ppValParen v2]
ppVal (Constr c Unit) = text c
ppVal (Constr c p) = hsep [text c, ppValParen p]
ppVal (Var x) = text x
ppVal (App func v) = hsep [ppFunc func, ppVal v]
ppVal (Minus v) = hcat [text "-", ppVal v]

ppValParen :: PVal -> Doc
ppValParen v = 
  case v of
    Unit     -> ppVal v
    Var _    -> ppVal v
    Pair _ _ -> PP.parens (ppVal v)
    _        -> ppVal v


ppFunc :: Func -> Doc
ppFunc (Func fname params) = hsep [ text fname, ppFParams params]
    where 
      ppFParams [] = empty
      ppFParams ((name, Nothing):params) = 
          hsep [ hcat [ text "~", text name], ppFParams params]
      ppFParams ((name, Just val):params) = 
          hsep [ hcat [ text "~", text name , text ":", ppFuncParens val] , 
                 ppFParams params]


ppFuncParens :: Func -> Doc 
ppFuncParens (Func fname []) = text fname 
ppFuncParens f = PP.parens (ppFunc f)


ppClause :: Clause -> Doc
ppClause (Clause p1 p2) = 
    hsep [text "|", pp p1, text "=", pp p2]
    where 
      pp (Just label, p) = hsep [text label, text "$", ppVal p]
      pp (Nothing, p) = ppVal p

instance Show PVal where 
    show p = show (ppVal p)

------------------------------------------------------------------------
-- Parsing

lexer :: GenTokenParser String a Identity
lexer = makeTokenParser $ 
        emptyDef { commentStart = "{-"
                 , commentEnd = "-}"
                 , commentLine = "--"
                 , identStart = letter <|> char '_'
                 , identLetter = alphaNum <|> char '_'
                 , reservedNames = ["data", "inL", "inR", "eval", "where", "iso"]
                 , reservedOpNames = ["+","*",",", ";", "=", ":", "()"]
                 , caseSensitive = True }
      
progParser :: Parsec String () Prog
progParser = do whiteSpace lexer 
                defs <- many defParser 
                eof
                return defs

defParser :: Parsec String () Def
defParser = 
    do t <- reserved lexer "data" >> identifier lexer
       symbol lexer "="
       args <- (sepBy (do ctor <- identifier lexer
                          typ <- option One typParser
                          return (ctor, typ))
                      (symbol lexer "|"))
       return (DataTyp t (List.sort args))
    <|>
    (do reserved lexer "iso"
        fname <- try (do fname <- identifier lexer
                         reserved lexer ":"
                         return fname)
        fparams <- many (try (do arg <- identifier lexer
                                 symbol lexer ":"
                                 ityp <- itypParser
                                 symbol lexer "->"
                                 return (arg, ityp)))
        ityp <- itypParser
        clauses <- many (do symbol lexer "|" 
                            p1 <- valParserWithLabel 
                            symbol lexer "="
                            p2 <- valParserWithLabel
                            return (Clause p1 p2))
        labels <- option [] -- now I need a "where" per label.
                  (do reserved lexer "where"
                      many1 (do label <- identifier lexer
                                symbol lexer ":" 
                                typ <- typParser
                                return (label, typ)))
        return (Iso fname fparams ityp labels clauses))
    <|>
    (do reserved lexer "eval" 
        func <- funcParser 
        v <- valParser 
        return (Eval func v))
    <|>
    (do reserved lexer "import" 
        modname <- uppercaseParse "module name"
        return (Import modname))


funcParser :: Parsec String () Func 
funcParser = parser
    where 
      simpleParser = 
          (do name <- lowercaseParse "iso name"
              return (Func name []))
          <|>
          (parens lexer parser)
      parser = 
          do name <- lowercaseParse "iso name"
             args <- many (do symbol lexer "~"
                              label <- lowercaseParse "label"
                              opt <- option Nothing 
                                     (do symbol lexer ":"
                                         arg <- simpleParser
                                         return (Just arg))
                              return (label, opt))
             return (Func name args)

typParser :: Parsec String () Typ 
typParser = buildExpressionParser typTable simpleTypParser
  where simpleTypParser = 
            (symbol lexer "0" >> return Zero) 
            <|> 
            (symbol lexer "1" >> return One) 
            <|> 
            (do t <- identifier lexer
                return (TName t))
            <|>
            (parens lexer typParser)
        typTable = 
            [[Prefix (reservedOp lexer "-" >> return Neg)],
             [Infix (reservedOp lexer "*" >> return Times) AssocLeft],
             [Infix (reservedOp lexer "+" >> return Plus) AssocLeft]]

valParser :: Parsec String () PVal
valParser = buildExpressionParser valTable simpleValParser
    where 
      simpleValParser = 
          (reservedOp lexer "()" >> return Unit)
          <|>
          (try (do fname <- funcParser; 
                   val <- simpleValParser
                   return (App fname val)))
          <|>
          (do name <- lowercaseParse "variable"
              return (Var name)) 
          <|>
          (do name <- uppercaseParse "constructor";
              val <- option Unit simpleValParser
              return (Constr name val))
          <|>
          (parens lexer valParser)
      valTable = 
          [[Prefix (do v <- reserved lexer "inL"; return LeftE),
            Prefix (do v <- reserved lexer "inR"; return RightE)],
           [Infix (reservedOp lexer "," >> return Pair) AssocLeft]]

valParserWithLabel :: Parsec String () (Maybe LName, PVal)
valParserWithLabel = do 
  do label <- option Nothing 
              (try (do l <- identifier lexer
                       symbol lexer "$"
                       return (Just l)))
     val <- valParser 
     return (label, val)

itypParser :: Parsec String () ITyp
itypParser = try parse 
             <|> parens lexer parse
    where 
      parse = typParser >>= \t1 -> 
              symbol lexer "=" >>
              typParser >>= \t2 -> 
              return (ITyp t1 t2)


startsWithUpper :: String -> Bool
startsWithUpper s = Char.isUpper (s !! 0) 

uppercaseParse :: String -> Parsec String () String
uppercaseParse msg = try (do name <- identifier lexer
                             if startsWithUpper name
                             then return name
                             else fail msg)

lowercaseParse :: String -> Parsec String () String
lowercaseParse msg = try (do name <- identifier lexer
                             if startsWithUpper name 
                             then fail msg
                             else return name)


------------------------------------------------------------------------
-- Coverage checker
 
type TypEnv = [(TName, [(Constr, Typ)])]
type CurTyp = (Typ, TypEnv)

lookup_exn v as = f (lookup v as)
    where 
      f (Just v) = v
      f Nothing = error ("can't find " ++ (show v))

matchesAny :: PVal -> Bool
matchesAny (Var _) = True
matchesAny (App _ _) = True
matchesAny _ = False

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
data CoverageError = NoMatches String PVal 
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
extend ((x,ls):env) (x1, val1) | x == x1 = (x, val1:ls):env
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


------------------------------------------------------------------------
-- Semantics

-- the value environment collects declared constants and isomorphisms
type VEnv = [(Var, Val)] 
type Clauses = [Clause]
newtype FEnv = FEnv [(FName, (Clauses, FEnv))]

data Dir = L | R 
           deriving Show
opp :: Dir -> Dir
opp L = R
opp R = L

projPattern :: Clause -> Dir -> (Maybe LName, PVal)
projPattern (Clause lhs _ ) L = lhs
projPattern (Clause _ rhs) R = rhs

projPattern2 :: Clause -> Dir -> ((Maybe LName, PVal), (Maybe LName, PVal))
projPattern2 cl dir = (projPattern cl dir, projPattern cl (opp dir))

projTyp :: ITyp -> Dir -> Typ
projTyp (ITyp lhs _) L = lhs
projTyp (ITyp _ rhs) R = rhs

data Cur = Cur {
      curDir :: Dir,
      curLabel :: Maybe LName,
      curVal :: Val
} deriving Show 

-- Deconstruct a value using a pattern. This builds an environment
-- binding the variables introduced by the pattern and their values.
deconst :: FEnv -> PVal -> Val -> VEnv -> Maybe VEnv 
deconst fenv Unit Unit env = Just env
deconst fenv (LeftE p) (LeftE v) env = deconst fenv p v env
deconst fenv (RightE p) (RightE v) env = deconst fenv p v env
deconst fenv (Pair p1 p2) (Pair v1 v2) env = f (deconst fenv p1 v1 env)
    where 
      f Nothing = Nothing
      f (Just env) = deconst fenv p2 v2 env 
deconst fenv (Constr c p) (Constr c1 v) env | c == c1 = deconst fenv p v env 
deconst fenv (Var x) v env = Just ((x, v):env)
deconst fenv (App func p) v env = 
    let (Cur {curVal=v'}) = evalFunc func fenv (Cur {curVal=v, curDir=R, curLabel=Nothing}) in 
    deconst fenv p v' env
deconst _ _ _ _ = Nothing 

-- Reconstruct a value from a pattern and an environment that supplies
-- the values of the variables in the pattern.
reconst :: FEnv -> VEnv -> PVal -> Maybe (VEnv, PVal)
reconst fenv env Unit = return (env, Unit)
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
reconst fenv env (Var x) = extract env x
    where 
      extract [] x = Nothing
      extract ((x1, v1):env) x | x1 == x = return (env, v1)
                               | otherwise = 
                                   do (env1, v) <- extract env x
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
      ret cl (Just ([], v)) = v
      ret cl Nothing = error $ "reconstruction failed " ++ (show cl) ++ " value " ++ (show cur)
      ret cl (Just (env, v)) = error $ "non-empty env returned : " ++ (show env) ++ " " ++ show cl ++ " " ++ show cur
      maybeIter cur@(Cur { curLabel = Nothing } ) = cur 
      maybeIter cur = evalClauses clauses fenv cur

evalFuncArgs :: Func -> FEnv -> (Clauses, FEnv)
evalFuncArgs (Func fname args) fenv = evalArgs args fenv
    where 
      evalArgs [] (FEnv env) = lookup_exn fname env
      evalArgs ((name, Nothing):args) fenv = evalArgs ((name, Just (Func name [])):args) fenv
      evalArgs ((name, Just func):args) fenv = 
          let closure = evalFuncArgs func fenv in
          let (clauses, FEnv fenv') = evalArgs args fenv in 
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
  
------------------------------------------------------------------------
-- Import handling

-- CR rjames: Now this is not exactly right eince all the defs are put
-- inot one big list. THis will cause different things to be availbale
-- in scope. For now I am going to ignore this. 
loadImports :: [Def] -> [String]-> IO [Def]
loadImports [] files = return []
loadImports (x@(DataTyp _ _) : rest) files =
  do defs <- loadImports rest files
     return $ x:defs
loadImports (x@(Iso _ _ _ _ _) : rest) files =
  do defs <- loadImports rest files
     return $ x:defs
loadImports (x@(Eval _ _) : rest) files =
  do defs <- loadImports rest files
     return $ x:defs
loadImports (x@(Import (f:fs)) : rest) files =
  do let filename = ((Char.toLower f) : fs) ++ ".ths"
     if elem filename files
     then do defs <- loadImports rest files           
             return $ defs
     else 
       do putStr $ "-- {Loading " ++ filename ++ "}\n";
          input <- readFile filename
          case runParser progParser () filename input of
            Left err ->
              do print err; 
                 return []
            Right newdefs ->
              do defs <- loadImports (newdefs ++ rest) (filename:files)            
                 return $ defs

------------------------------------------------------------------------
-- Read, process, print 

type Filename = String

repIO :: Filename -> (Prog -> IO ()) -> IO ()
repIO ifile processProg = 
  do input <- readFile ifile
     either print processProg (runParser progParser () ifile input)

repF :: Show a => Filename -> Filename -> (Prog -> a) -> IO ()
repF ifile ofile processProg = 
  repIO ifile (\prog -> writeFile ofile (show (processProg prog)))

-- just echo
echo :: Filename -> IO ()
echo ifile = repIO ifile (print . ppProg)

-- typecheck and echo
echoT :: Filename -> IO ()
echoT ifile = repIO ifile 
              (\p -> do defs <- loadImports p [ifile]
                        print (ppProg defs)
                        reportErrors (tProg defs))

-- typecheck and evaluate
run :: Filename -> IO ()
run ifile = repIO ifile $ \init -> 
            do p <- loadImports init [ifile]
               putStr "Typechecking...\n"
               case tProg p of 
                 [] -> do putStr "Evaluating...\n"
                          (_fenv, res) <- evalProg p (FEnv []) True
                          print_each res
                 errs -> reportErrors errs
    where
      print_each [] = return () 
      print_each ((func, val, res):rs) = 
          do putStr $ "eval " ++ show (ppFunc func) ++ " " ++ show (ppVal val) 
                        ++ " = " ++ show (curVal res) ++ "\n"
             print_each rs


------------------------------------------------------------------------

