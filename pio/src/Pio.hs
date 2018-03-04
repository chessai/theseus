-- Language of pure isomorphisms with trace and recursive types

module Pio where

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import qualified Control.Monad.State as ST

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
-- Abstract syntax

type Prog = [Def]

data Def = DataSyn String VTyp    -- e.g.: data Bool = 1 + 1
         | Const String VTyp Val  -- e.g.: const false : Bool = ...
         | Iso String ITyp Iso    -- e.g.: iso not : Bool <-> Bool = ...
         | Main String Val        -- e.g.: main = not @ false

data VTyp = Zero
          | One
          | Plus VTyp VTyp
          | Times VTyp VTyp
          | TName String          -- refers to a declared type
          | TVar Integer          -- used for unification

data Val = Unit
         | Var String             -- refers to a declared constant
         | LeftE Val
         | RightE Val
         | Pair Val Val
         | Rec String Val         -- folded recursive values of given type
         | Clos (Val -> Val)      -- compiled isos

data ITyp = ITyp VTyp VTyp

data Iso = ZeroE | ZeroI | SwapPlus | AssocLPlus | AssocRPlus
         | UnitE | UnitI | SwapTimes | AssocLTimes | AssocRTimes
         | Distrib0 | Factor0 | Distrib | Factor
         | Fold String | Unfold String
         | Id | SymI Iso | SeqI Iso Iso
         | SumI Iso Iso | ProdI Iso Iso | Trace Iso
         | VarA String            -- declared isos
          
------------------------------------------------------------------------
-- Pretty printing

ppProg :: Prog -> Doc
ppProg []     = PP.space
ppProg (d:ds) = PP.space $+$ ppDef d $+$ ppProg ds

ppDef :: Def -> Doc
ppDef (DataSyn t typ) = 
  hsep [text "data", text t, equals, ppVTyp typ]
ppDef (Const c typ v) = 
  hsep [text "const", text c, text ":", ppVTyp typ, equals, ppVal v]
ppDef (Iso c typ e) = 
  hsep [text "iso", text c, text ":", ppITyp typ, equals, ppIso e]
ppDef (Main c v) = 
  hsep [text "main", equals, text c, text "@", ppVal v]

ppVTyp :: VTyp -> Doc
ppVTyp Zero          = text "0"
ppVTyp One           = text "1"
ppVTyp (Plus t1 t2)  = hsep [ppVTypParen t1, text "+", ppVTypParen t2]
ppVTyp (Times t1 t2) = hsep [ppVTypParen t1, text "*", ppVTypParen t2]
ppVTyp (TName t)     = text t
ppVTyp (TVar n)      = text "%b" <> PP.integer n

ppVTypParen :: VTyp -> Doc
ppVTypParen t = 
  case t of 
    Plus _ _  -> PP.parens (ppVTyp t)
    Times _ _ -> PP.parens (ppVTyp t)
    _         -> ppVTyp t

ppVal :: Val -> Doc
ppVal Unit         = text "tt"
ppVal (Var s)      = text s
ppVal (LeftE v)    = hsep [text "inL", ppValParen v]
ppVal (RightE v)   = hsep [text "inR", ppValParen v]
ppVal (Pair v1 v2) = sep [hsep [text "<", ppVal v1, PP.comma],
                          nest 2 $ hsep [ppVal v2, text ">"]]
ppVal (Rec s v)    = hsep [text "fold", text s, ppValParen v]
ppVal (Clos _)     = text "%CLOSURE"
                                         
ppValParen :: Val -> Doc
ppValParen v = 
  case v of
    Unit     -> ppVal v
    Var _    -> ppVal v
    Pair _ _ -> ppVal v
    Clos _   -> ppVal v
    _        -> PP.parens (ppVal v)

ppITyp :: ITyp -> Doc
ppITyp (ITyp b1 b2) = hsep [ppVTyp b1, text "<->", ppVTyp b2]
    
ppIso :: Iso -> Doc
ppIso ZeroE         = text "zeroe"
ppIso ZeroI         = text "zeroi"
ppIso SwapPlus      = text "swap+"
ppIso AssocLPlus    = text "assocL+"
ppIso AssocRPlus    = text "assocR+"
ppIso UnitE         = text "unite"
ppIso UnitI         = text "uniti"
ppIso SwapTimes     = text "swap*"
ppIso AssocLTimes   = text "assocL*"
ppIso AssocRTimes   = text "assocR*"
ppIso Distrib0      = text "distrib0"
ppIso Factor0       = text "factor0"
ppIso Distrib       = text "distrib"
ppIso Factor        = text "factor"
ppIso (Fold s)      = hsep [text "fold", text s]
ppIso (Unfold s)    = hsep [text "unfold", text s]
ppIso Id            = text "id"
ppIso (SymI c)      = hsep [text "sym", ppIsoParen c]
ppIso (SeqI c1 c2)  = (ppIsoParen2 c1 <+> text ";") $$
                      (ppIsoParen2 c2)
ppIso (SumI c1 c2)  = hsep [ppIsoParen c1, text "+", ppIsoParen c2]
ppIso (ProdI c1 c2) = hsep [ppIsoParen c1, text "*", ppIsoParen c2]
ppIso (Trace c)     = hsep [text "trace", ppIsoParen c]
ppIso (VarA s)      = text s

ppIsoParen :: Iso -> Doc
ppIsoParen c = 
  case c of 
    SymI _    -> PP.parens (ppIso c)
    SeqI _ _  -> PP.parens (ppIso c)
    SumI _ _  -> PP.parens (ppIso c)
    ProdI _ _ -> PP.parens (ppIso c)
    Trace _   -> PP.parens (ppIso c)
    _         -> ppIso c

ppIsoParen2 :: Iso -> Doc
ppIsoParen2 c = 
  case c of 
    SumI _ _  -> PP.parens (ppIso c)
    ProdI _ _ -> PP.parens (ppIso c)
    _         -> ppIso c

------------------------------------------------------------------------
-- Parsing

lexer :: GenTokenParser String a Identity
lexer = makeTokenParser $ 
        emptyDef { commentStart = "{-"
                 , commentEnd = "-}"
                 , commentLine = "--"
                 , identStart = letter <|> char '_'
                 , identLetter = alphaNum <|> char '_'
                 , reservedNames = ["data", "const", "iso", "main",
                                    "inL", "inR", 
                                    "zeroe", "zeroi", "swap+",
                                    "assocl+", "assocr+",
                                    "unite", "uniti", "swap*",
                                    "assocl*", "assocr*",
                                    "distrib0", "factor0",
                                    "distrib", "factor",
                                    "fold", "unfold",
                                    "id", "sym", "trace"]
                 , reservedOpNames = ["+","*",";"]
                 , caseSensitive = True }
      
progParser :: Parsec String () Prog
progParser = whiteSpace lexer >> many defParser

defParser :: Parsec String () Def
defParser = 
    (reserved lexer "data" >> identifier lexer >>= \t -> 
     symbol lexer "=" >> typParser >>= \typ -> 
     return (DataSyn t typ))
    <|>
    (reserved lexer "const" >> identifier lexer >>= \c -> 
     symbol lexer ":" >> typParser >>= \typ -> 
     symbol lexer "=" >> valParser >>= \v -> 
     return (Const c typ v))
    <|>
    (reserved lexer "iso" >> identifier lexer >>= \c -> 
     symbol lexer ":" >> itypParser >>= \t -> 
     symbol lexer "=" >> isoParser >>= \body -> 
     return (Iso c t body))
    <|>
    (reserved lexer "main" >> symbol lexer "=" >> 
     identifier lexer >>= \c -> 
     symbol lexer "@" >> 
     valParser >>= \v -> 
     eof >> return (Main c v))

typParser :: Parsec String () VTyp 
typParser = buildExpressionParser typTable simpleTypParser
  where simpleTypParser = 
            (symbol lexer "0" >> return Zero) 
            <|> 
            (symbol lexer "1" >> return One) 
            <|> 
            (identifier lexer >>= \t -> 
             return (TName t))
            <|>
            (parens lexer typParser)
        typTable = 
            [[Infix (reservedOp lexer "*" >> return Times) AssocLeft],
             [Infix (reservedOp lexer "+" >> return Plus) AssocLeft]]

valParser :: Parsec String () Val
valParser = 
  (symbol lexer "tt" >> return Unit)
  <|>
  (identifier lexer >>= return . Var) 
  <|>
  (reserved lexer "inL" >> valParser >>= \v -> 
   return (LeftE v)) 
  <|>
  (reserved lexer "inR" >> valParser >>= \v -> 
   return (RightE v)) 
  <|>
  (symbol lexer "<" >> valParser >>= \v1 -> 
   symbol lexer "," >> valParser >>= \v2 -> 
   symbol lexer ">" >>
   return (Pair v1 v2)) 
  <|>
  (reserved lexer "fold" >> 
   identifier lexer >>= \s -> 
   valParser >>= \v -> 
   return (Rec s v))
  <|> 
  (parens lexer valParser)

itypParser :: Parsec String () ITyp
itypParser = 
    typParser >>= \t1 -> 
    symbol lexer "<->" >>
    typParser >>= \t2 -> 
    return (ITyp t1 t2)

isoParser :: Parsec String () Iso
isoParser = buildExpressionParser isoTable simpleIsoParser
  where simpleIsoParser = 
          (reserved lexer "zeroe" >> return ZeroE)
          <|>
          (reserved lexer "zeroi" >> return ZeroI)
          <|>
          (reserved lexer "swap+" >> return SwapPlus)
          <|>
          (reserved lexer "assocl+" >> return AssocLPlus)
          <|>
          (reserved lexer "assocr+" >> return AssocRPlus)
          <|>
          (reserved lexer "unite" >> return UnitE)
          <|>
          (reserved lexer "uniti" >> return UnitI)
          <|>
          (reserved lexer "swap*" >> return SwapTimes)
          <|>
          (reserved lexer "assocl*" >> return AssocLTimes)
          <|>
          (reserved lexer "assocr*" >> return AssocRTimes)
          <|>
          (reserved lexer "distrib0" >> return Distrib0)
          <|>
          (reserved lexer "factor0" >> return Factor0)
          <|>
          (reserved lexer "distrib" >> return Distrib)
          <|>
          (reserved lexer "factor" >> return Factor)
          <|>
          (reserved lexer "fold" >> 
           identifier lexer >>= \s -> 
           return (Fold s))
          <|>
          (reserved lexer "unfold" >> 
           identifier lexer >>= \s -> 
           return (Unfold s))
          <|>
          (reserved lexer "id" >> return Id)
          <|>
          (reserved lexer "sym" >> isoParser >>= return . SymI)
          <|>
          (reserved lexer "trace" >> isoParser >>= return . Trace)
          <|>
          (identifier lexer >>= return . VarA)
          <|> 
          (parens lexer isoParser)
        isoTable = 
            [[Infix (reservedOp lexer "*" >> return ProdI) AssocLeft],
             [Infix (reservedOp lexer "+" >> return SumI) AssocLeft],
             [Infix (reservedOp lexer ";" >> return SeqI) AssocRight]]
                   
------------------------------------------------------------------------
-- Substitutions, etc. 

-- signature for keeping track of type synonyms, declared constants, and
-- declared isos. 

data SigEntry = SigT String VTyp 
              | SigC String VTyp
              | SigI String ITyp

type Sig = [SigEntry] 

emptySig :: Sig
emptySig = []

lookupSigT :: String -> Sig -> Maybe VTyp
lookupSigT s []                            = Nothing
lookupSigT s (SigT s' t : sig) | s == s'   = Just t
                               | otherwise = lookupSigT s sig
lookupSigT s (_ : sig)                     = lookupSigT s sig

lookupSigC :: String -> Sig -> Maybe VTyp
lookupSigC s []                            = Nothing
lookupSigC s (SigC s' t : sig) | s == s'   = Just t
                               | otherwise = lookupSigC s sig
lookupSigC s (_ : sig)                     = lookupSigC s sig

lookupSigI :: String -> Sig -> Maybe ITyp
lookupSigI s []                            = Nothing
lookupSigI s (SigI s' t : sig) | s == s'   = Just t
                               | otherwise = lookupSigI s sig
lookupSigI s (_ : sig)                     = lookupSigI s sig

-- substitution for unification

type Sub = [(Integer,VTyp)]

emptySub :: Sub
emptySub = []

-- state is the index of the next logical variable and the substitution

type S   = (Integer,Sub)

walk :: Sub -> VTyp -> VTyp
walk sub (TVar n) = maybe (TVar n) (walk sub) (lookup n sub)
walk sub b = b

walkVTyp :: Sub -> VTyp -> VTyp
walkVTyp sub b = 
  case walk sub b of 
    Zero        -> Zero
    One         -> One
    Plus b1 b2  -> Plus (walkVTyp sub b1) (walkVTyp sub b2)
    Times b1 b2 -> Times (walkVTyp sub b1) (walkVTyp sub b2)
    TName s     -> TName s
    TVar n      -> TVar n

walkITyp :: Sub -> ITyp -> ITyp
walkITyp sub (ITyp b1 b2) = ITyp (walkVTyp sub b1) (walkVTyp sub b2)

newTVar :: ST.State S VTyp
newTVar = do (alpha,subst) <- ST.get
             ST.put (alpha+1,subst)
             return (TVar alpha)

unify :: Doc -> Sig -> VTyp -> VTyp -> ST.State S ()
unify doc sig b1 b2 = 
  do (alpha,subst) <- ST.get
     let b1' = walk subst b1
     let b2' = walk subst b2
     case (b1',b2') of
       (Zero,Zero)                -> return ()
       (One,One)                  -> return ()
       (Plus t1 t2, Plus u1 u2)   -> do unify doc sig t1 u1; unify doc sig t2 u2
       (Times t1 t2, Times u1 u2) -> do unify doc sig t1 u1; unify doc sig t2 u2
       (TName a, TName b)         -> 
           if a == b 
           then return () 
           else error ("Cannot unify types " ++ a ++ " and " ++ b 
                      ++ " in the definition " ++ show doc)
       (TVar n1,TVar n2)          -> 
           if n1 == n2 then return ()
           else ST.put (alpha,(n1,b2'):subst)
       (TVar n1,b2') -> ST.put (alpha,(n1,b2'):subst)
       (b1',TVar n2) -> ST.put (alpha,(n2,b1'):subst)
       _                          -> 
           error (printf 
                  "In %s:\n\tTypes %s and %s do not unify"
                  (show doc) (show (ppVTyp b1')) (show (ppVTyp b2')))

------------------------------------------------------------------------
-- Typing

tProg :: Prog -> ()
tProg prog = ST.evalState (checkProg prog emptySig) (0,emptySub)

checkProg :: [Def] -> Sig -> ST.State S ()
checkProg [d@(Main s v)] sig = 
  case lookupSigI s sig of
    Nothing           -> error ("checkProg: unknown function symbol " ++ s)
    Just (ITyp b1 b2) -> do b <- inferVal v sig
                            unify (ppDef d) sig b1 b
checkProg (d:ds) sig     = 
  do sig' <- inferDef d sig 
     checkProg ds sig'

inferDef :: Def -> Sig -> ST.State S Sig
inferDef (DataSyn s b) sig  = 
  return (SigT s b : sig)
inferDef d@(Const s b v) sig = 
  do b' <- inferVal v sig
     unify (ppDef d) sig b b'
     return (SigC s b : sig)
inferDef (d@(Iso f (ITyp b1 b2) c)) sig = 
  do ITyp b1' b2' <- inferITyp sig c
     unify (ppDef d) sig b1 b1'
     unify (ppDef d) sig b2 b2'
     return (SigI f (ITyp b1 b2) : sig)

inferVal :: Val -> Sig -> ST.State S VTyp
inferVal Unit _           = return One
inferVal (Var s) sig      = case lookupSigC s sig of 
  Nothing -> error ("inferVal: unknown value name " ++ s)
  Just t -> return t
inferVal (LeftE v) sig    = do b1 <- inferVal v sig
                               b2 <- newTVar
                               return (Plus b1 b2)
inferVal (RightE v) sig   = do b1 <- newTVar
                               b2 <- inferVal v sig
                               return (Plus b1 b2)
inferVal (Pair v1 v2) sig = do b1 <- inferVal v1 sig
                               b2 <- inferVal v2 sig
                               return (Times b1 b2)
inferVal (d@(Rec s v)) sig    = 
    case lookupSigT s sig of 
      Nothing -> error ("inferVal: unknown recursive type " ++ s)
      Just t -> do b <- inferVal v sig
                   unify (ppVal d) sig b t
                   return (TName s)

inferITyp :: Sig -> Iso -> ST.State S ITyp
inferITyp sig (VarA s) = 
  case lookupSigI s sig of
    Nothing -> error ("inferITyp: unknown function symbol " ++ s)
    Just t  -> return t
inferITyp sig ZeroE =
  do b <- newTVar
     return (ITyp (Plus Zero b) b)
inferITyp sig ZeroI =
  do b <- newTVar
     return (ITyp b (Plus Zero b))
inferITyp sig SwapPlus = 
  do b0 <- newTVar
     b1 <- newTVar
     return (ITyp (Plus b0 b1) (Plus b1 b0))
inferITyp sig AssocLPlus = 
  do b0 <- newTVar
     b1 <- newTVar
     b2 <- newTVar
     return (ITyp
             (Plus b0 (Plus b1 b2))
             (Plus (Plus b0 b1) b2))
inferITyp sig AssocRPlus = 
  do b0 <- newTVar
     b1 <- newTVar
     b2 <- newTVar
     return (ITyp
             (Plus (Plus b0 b1) b2)
             (Plus b0 (Plus b1 b2)))
inferITyp sig UnitE = 
  do b <- newTVar
     return (ITyp (Times One b) b)
inferITyp sig UnitI = 
  do b <- newTVar
     return (ITyp b (Times One b))
inferITyp sig SwapTimes = 
  do b0 <- newTVar
     b1 <- newTVar
     return (ITyp (Times b0 b1) (Times b1 b0))
inferITyp sig AssocLTimes = 
  do b0 <- newTVar
     b1 <- newTVar
     b2 <- newTVar
     return (ITyp
             (Times b0 (Times b1 b2))
             (Times (Times b0 b1) b2))
inferITyp sig AssocRTimes = 
  do b0 <- newTVar
     b1 <- newTVar
     b2 <- newTVar
     return (ITyp
             (Times (Times b0 b1) b2)
             (Times b0 (Times b1 b2)))
inferITyp sig Distrib0 = 
  do b <- newTVar
     return (ITyp (Times Zero b) Zero)
inferITyp sig Factor0 = 
  do b <- newTVar
     return (ITyp Zero (Times Zero b))
inferITyp sig Distrib = 
  do b0 <- newTVar
     b1 <- newTVar
     b2 <- newTVar
     return (ITyp
             (Times (Plus b0 b1) b2)
             (Plus (Times b0 b2) (Times b1 b2)))
inferITyp sig Factor = 
  do b0 <- newTVar
     b1 <- newTVar
     b2 <- newTVar
     return (ITyp
             (Plus (Times b0 b2) (Times b1 b2))
             (Times (Plus b0 b1) b2))
inferITyp sig (Fold s) = 
    case lookupSigT s sig of 
      Nothing -> error ("Unknown recursive type " ++ s)
      Just t -> return (ITyp t (TName s))
inferITyp sig (Unfold s) = 
    case lookupSigT s sig of 
      Nothing -> error ("Unknown recursive type " ++ s)
      Just t -> return (ITyp (TName s) t)
inferITyp sig Id = 
  do b <- newTVar
     return (ITyp b b)
inferITyp sig (SymI c) =        
  do ITyp b1 b2 <- inferITyp sig c
     return (ITyp b2 b1)
inferITyp sig d@(SeqI c1 c2) = 
  do ITyp b1 b2 <- inferITyp sig c1
     ITyp b2' b3 <- inferITyp sig c2
     unify (ppIso d) sig b2 b2'
     return (ITyp b1 b3)
inferITyp sig (SumI c1 c2) = 
  do ITyp b1 b2 <- inferITyp sig c1
     ITyp b3 b4 <- inferITyp sig c2
     return (ITyp (Plus b1 b3) (Plus b2 b4))
inferITyp sig (ProdI c1 c2) = 
  do ITyp b1 b2 <- inferITyp sig c1
     ITyp b3 b4 <- inferITyp sig c2
     return (ITyp (Times b1 b3) (Times b2 b4))
inferITyp sig d@(Trace c) = 
  do ITyp (Plus c1 a) (Plus c2 b) <- inferITyp sig c
     unify (ppIso d) sig c1 c2
     return (ITyp a b)

------------------------------------------------------------------------
-- Semantics

-- the value environment collects declared constants and isomorphisms
type VEnv = [(String,Val)] 

emptyEnv :: VEnv
emptyEnv = []

evalProg :: [Def] -> VEnv -> Val
evalProg [Main f v] env = 
  case lookup f env of 
    Nothing          -> error ("evalProg: unknown function symbol " ++ f)
    Just (Clos clos) -> clos (evalV v env)
evalProg (d:ds) env     = evalProg ds (evalDef d env)

evalDef :: Def -> VEnv -> VEnv
evalDef (DataSyn _ _) env    = env
evalDef (Const s _ val) env = (s , evalV val env) : env
evalDef (Iso f _ c) env     = (f , Clos (evalR env c)) : env
             
evalV :: Val -> VEnv -> Val
evalV Unit _           = Unit
evalV (Var s) env      = case lookup s env of
  Nothing -> error ("evalV: unknown constant" ++ s)
  Just v  -> v
evalV (LeftE v) env    = LeftE (evalV v env) 
evalV (RightE v) env   = RightE (evalV v env) 
evalV (Pair v1 v2) env = Pair (evalV v1 env) (evalV v2 env)
evalV (Rec s v) env    = Rec s (evalV v env)

evalR :: VEnv -> Iso -> (Val -> Val)
evalR env (VarA s) v = case lookup s env of
  Nothing          -> error ("evalR: unknown function " ++ s)
  Just (Clos clos) -> clos v
evalR env ZeroE (RightE v)                   = v
evalR env ZeroI v                            = RightE v
evalR env SwapPlus (LeftE v)                 = RightE v
evalR env SwapPlus (RightE v)                = LeftE v
evalR env AssocLPlus (LeftE v)               = LeftE (LeftE v)
evalR env AssocLPlus (RightE (LeftE v))      = LeftE (RightE v)
evalR env AssocLPlus (RightE (RightE v))     = RightE v
evalR env AssocRPlus (LeftE (LeftE v))       = LeftE v
evalR env AssocRPlus (LeftE (RightE v))      = RightE (LeftE v)
evalR env AssocRPlus (RightE v)              = RightE (RightE v)
evalR env UnitE (Pair Unit v)                = v
evalR env UnitI v                            = Pair Unit v
evalR env SwapTimes (Pair v1 v2)             = Pair v2 v1
evalR env AssocLTimes (Pair v1 (Pair v2 v3)) = Pair (Pair v1 v2) v3
evalR env AssocRTimes (Pair (Pair v1 v2) v3) = Pair v1 (Pair v2 v3)
evalR env Distrib0 _                         = error "Emtpy type"
evalR env Factor0 _                          = error "Empty type"
evalR env Distrib (Pair (LeftE v1) v2)       = LeftE (Pair v1 v2)
evalR env Distrib (Pair (RightE v1) v2)      = RightE (Pair v1 v2)
evalR env Factor (LeftE (Pair v1 v2))        = Pair (LeftE v1) v2
evalR env Factor (RightE (Pair v1 v2))       = Pair (RightE v1) v2
evalR env (Fold s) v                         = Rec s v
evalR env (Unfold s) (Rec s' v)              = v -- s and s' the same
evalR env Id v                               = v
evalR env (SymI c) v                         = evalRB env c v
evalR env (SeqI c1 c2) v                     = evalR env c2 (evalR env c1 v)
evalR env (SumI c1 c2) (LeftE v)             = LeftE (evalR env c1 v) 
evalR env (SumI c1 c2) (RightE v)            = RightE (evalR env c2 v) 
evalR env (ProdI c1 c2) (Pair v1 v2)         = 
  Pair (evalR env c1 v1) (evalR env c2 v2)
evalR env (Trace c) v                        = loop (evalR env c (RightE v))
  where loop (RightE a) = a
        loop (LeftE a)  = loop (evalR env c (LeftE a))
evalR env c _ = error ("evalR: don't know how to interpret " ++ 
                       show (ppIso c))

evalRB :: VEnv -> Iso -> (Val -> Val)
evalRB env (VarA s) v = case lookup s env of
  Nothing          -> error ("evalRB: unknown function " ++ s)
  Just (Clos clos) -> clos v
evalRB env ZeroE v                            = RightE v
evalRB env ZeroI (RightE v)                   = v
evalRB env SwapPlus (LeftE v)                 = RightE v
evalRB env SwapPlus (RightE v)                = LeftE v
evalRB env AssocLPlus (LeftE (LeftE v))       = LeftE v
evalRB env AssocLPlus (LeftE (RightE v))      = RightE (LeftE v)
evalRB env AssocLPlus (RightE v)              = RightE (RightE v)
evalRB env AssocRPlus (LeftE v)               = LeftE (LeftE v)
evalRB env AssocRPlus (RightE (LeftE v))      = LeftE (RightE v)
evalRB env AssocRPlus (RightE (RightE v))     = RightE v
evalRB env UnitE v                            = Pair Unit v
evalRB env UnitI (Pair Unit v)                = v
evalRB env SwapTimes (Pair v1 v2)             = Pair v2 v1
evalRB env AssocLTimes (Pair (Pair v1 v2) v3) = Pair v1 (Pair v2 v3)
evalRB env AssocRTimes (Pair v1 (Pair v2 v3)) = Pair (Pair v1 v2) v3
evalRB env Distrib0 _                         = error "Emtpy type"
evalRB env Factor0 _                          = error "Empty type"
evalRB env Distrib (LeftE (Pair v1 v2))       = Pair (LeftE v1) v2
evalRB env Distrib (RightE (Pair v1 v2))      = Pair (RightE v1) v2
evalRB env Factor (Pair (LeftE v1) v2)        = LeftE (Pair v1 v2)
evalRB env Factor (Pair (RightE v1) v2)       = RightE (Pair v1 v2)
evalRB env (Fold s) (Rec s' v)                = v  -- s and s' the same
evalRB env (Unfold s) v                       = Rec s v
evalRB env Id v                               = v
evalRB env (SymI c) v                         = evalR env c v
evalRB env (SeqI c1 c2) v                     = evalRB env c2 (evalRB env c1 v)
evalRB env (SumI c1 c2) (LeftE v)             = LeftE (evalRB env c1 v) 
evalRB env (SumI c1 c2) (RightE v)            = RightE (evalRB env c2 v) 
evalRB env (ProdI c1 c2) (Pair v1 v2)         = 
  Pair (evalRB env c1 v1) (evalRB env c2 v2)
evalRB env (Trace c) v                        = loop (evalRB env c (RightE v))
  where loop (RightE a) = a
        loop (LeftE a)  = loop (evalRB env c (LeftE a))
evalRB env c _ = error ("evalRB: don't know how to interpret " ++ 
                       show (ppIso c))

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
echoT ifile = repIO ifile (\p -> seq (tProg p) (print (ppProg p)))

-- typecheck and evaluate

run :: Filename -> IO ()
run ifile = repIO ifile $ \p -> 
            do putStr "Typechecking...\n"
               seq (tProg p) (return ())
               putStr "Evaluating...\n"
               print (ppVal (evalProg p emptyEnv))

------------------------------------------------------------------------
