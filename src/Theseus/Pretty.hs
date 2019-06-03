{-# language LambdaCase #-}

module Theseus.Pretty
  ( ppProg
  , ppDef
  , ppFunc
  , ppVal
  ) where

import Text.PrettyPrint.HughesPJ hiding (char,comma,parens,integer,space)
import qualified Text.PrettyPrint.HughesPJ as PP

import Theseus.AbstractSyntax

ppProg :: Prog -> Doc
ppProg = \case
  [] -> PP.space
  (d:ds) -> PP.space $+$ ppDef d $+$ ppProg ds

ppDef :: Def -> Doc
ppDef = \case
  DataTyp t alts ->
    hsep [text "data", text t, equals, ppAlts alts]
  Iso name args ityp labels clauses ->
    vcat
      [ hsep [text "iso", text name, text ":", ppFArgs args, ppITyp ityp]
      , vcat (map ppClause clauses)
      , ppLabels labels
      ]
  Eval func val ->
    hsep [text "eval", ppFunc func, ppVal val]
  Import modname ->
    hsep [text "use", text modname]

ppAlts :: [(Constr,Typ)] -> Doc
ppAlts = \case
  [] -> PP.space
  [(constr,One)] ->
    hsep [text constr] -- drop unit args of ctors
  [(constr,typ)] ->
    hsep [text constr, ppTyp typ]
  ((constr,One):alts) ->
    hsep [text constr, text "|", ppAlts alts]
  ((constr,typ):alts) ->
    hsep [text constr, ppTyp typ, text "|", ppAlts alts]

ppFArgs :: [(FName, ITyp)] -> Doc
ppFArgs = \case
  [] -> empty
  ((fname, ityp):args) ->
    hsep [ hcat [text fname, text ":", text "(", ppITyp ityp, text ")"]
         , text "->", ppFArgs args
         ]

ppTyp :: Typ -> Doc
ppTyp = \case
  One -> text "1"
  Zero -> text "0"
  TName t -> text t
  Neg t -> hcat [text "-", ppTyp t]
  Times t1 t2 -> hsep [ppTypParen t1, text "*", ppTypParen t2]
  Plus t1 t2 -> hsep [ppTypParen t1, text "+", ppTypParen t2]

ppITyp :: ITyp -> Doc
ppITyp (ITyp b1 b2) = hsep [ppTyp b1, text "=", ppTyp b2]


ppTypParen :: Typ -> Doc
ppTypParen = \case
  t@Times{} -> PP.parens (ppTyp t)
  t@Plus{} -> PP.parens (ppTyp t)
  t -> ppTyp t

ppLabels :: [(LName, Typ)] -> Doc
ppLabels = \case
  [] -> empty
  ((label, typ):labels) ->
    vcat [ hsep [text "where", text label, text ":", ppTyp typ]
         , ppLabels labels
         ]

ppVal :: PVal -> Doc
ppVal = \case
  Unit -> text "()"
  LeftE v -> hsep [text "inL", ppValParen v]
  RightE v -> hsep [text "inR", ppValParen v]
  Pair v1 v2 -> sep [ hcat [ppVal v1, PP.comma]
                    , nest 2 $ ppValParen v2
                    ]
  Constr c Unit -> text c
  Constr c p -> hsep [text c, ppValParen p]
  Var x -> text x
  App func v -> hsep [ppFunc func, ppVal v]
  Minus v -> hcat [text "-", ppVal v]

ppValParen :: PVal -> Doc
ppValParen = \case
  v@Pair{} -> PP.parens (ppVal v)
  v -> ppVal v


ppFunc :: Func -> Doc
ppFunc (Func fname params) = hsep [ text fname, ppFParams params]
  where
    ppFParams [] = empty
    ppFParams ((name, Nothing):params') =
      hsep [ hcat [ text "~", text name], ppFParams params']
    ppFParams ((name, Just val):params') =
      hsep [ hcat [ text "~", text name , text ":", ppFuncParens val]
           , ppFParams params'
           ]

ppFuncParens :: Func -> Doc
ppFuncParens = \case
  Func fname [] -> text fname
  f -> PP.parens (ppFunc f)

ppClause :: Clause -> Doc
ppClause (Clause p1 p2) = hsep [text "|", pp p1, text "=", pp p2]
  where
    pp = \case
      (Nothing, p) -> ppVal p
      (Just label, p) -> hsep [text label, text "$", ppVal p]


