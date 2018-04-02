{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wall #-}
-- this is bad
{-# OPTIONS_GHC -fno-warn-orphans #-} 

module Theseus.PrettyPrint where

import Theseus.AbstractSyntax
import Text.PrettyPrint.HughesPJ hiding (char,comma,parens,integer,space)
import qualified Text.PrettyPrint.HughesPJ as PP (comma,parens,space)

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
      ppFParams ((name, Nothing):params') =
          hsep [ hcat [ text "~", text name], ppFParams params']
      ppFParams ((name, Just val):params') =
          hsep [ hcat [ text "~", text name , text ":", ppFuncParens val] ,
                 ppFParams params']


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

deriving instance Show Clause
