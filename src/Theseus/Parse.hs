module Theseus.Parse
  ( progParser
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
import Theseus.Pretty

------------------------------------------------------------------------
-- Parsing

lexer :: GenTokenParser String a Identity
lexer = makeTokenParser $
  emptyDef { commentStart = "{-"
           , commentEnd = "-}"
           , commentLine = "#"
           , identStart = letter <|> char '_'
           , identLetter = alphaNum <|> char '_'
           , reservedNames = ["data", "inL", "inR", "eval", "where", "iso"]
           , reservedOpNames = ["+","*",",", ";", "=", ":", "()"]
           , caseSensitive = True
           }

progParser :: Parsec String () Prog
progParser = do
  whiteSpace lexer
  defs <- many defParser
  eof
  return defs

dataParser :: Parsec String () Def
dataParser = do
  t <- reserved lexer "data" >> identifier lexer
  symbol lexer "="
  args <- sepBy
    (do ctor <- identifier lexer
        typ <- option One typParser
        return (ctor, typ))
    (symbol lexer "|")
  pure (DataTyp t (List.sort args))

isoParser :: Parsec String () Def
isoParser = do
  reserved lexer "iso"
  fname <- try $ do
    fname <- identifier lexer
    reserved lexer ":"
    return fname
  fparams <- many $ try $ do
    arg <- identifier lexer
    symbol lexer ":"
    ityp <- itypParser
    symbol lexer "->"
    return (arg, ityp)
  ityp <- itypParser
  clauses <- many $ do
    symbol lexer "|"
    p1 <- valParserWithLabel
    symbol lexer "="
    p2 <- valParserWithLabel
    return (Clause p1 p2)
  labels <- option [] $ do -- now I need a "where" per label.
    reserved lexer "where"
    many1 $ do
      label <- identifier lexer
      symbol lexer ":"
      typ <- typParser
      return (label, typ)
  return (Iso fname fparams ityp labels clauses)

evalParser :: Parsec String () Def
evalParser = do
  reserved lexer "eval"
  func <- funcParser
  v <- valParser
  return (Eval func v)

importParser :: Parsec String () Def
importParser = do
  reserved lexer "use"
  modname <- uppercaseParse "module name"
  return (Import modname)

defParser :: Parsec String () Def
defParser = asum
  [ dataParser
  , isoParser
  , evalParser
  , importParser
  ]

funcParser :: Parsec String () Func
funcParser = do
  name <- lowercaseParse "iso name"
  args <- many $ do
    symbol lexer "~"
    label <- lowercaseParse "label"
    opt <- option Nothing $ do
      symbol lexer ":"
      arg <- simpleParser
      return (Just arg)
    return (label, opt)
  return (Func name args)
  where
    simpleParser =
      (do name <- lowercaseParse "iso name"
          return (Func name []))
      <|> (parens lexer funcParser)

typParser :: Parsec String () Typ
typParser = buildExpressionParser typTable simpleTypParser
  where
    simpleTypParser =
      (symbol lexer "0" $> Zero)
      <|>
      (symbol lexer "1" $> One)
      <|>
      (TName <$> identifier lexer)
      <|>
      (parens lexer typParser)
    typTable =
      [ [ Prefix (reservedOp lexer "-" $> Neg)]
      , [ Infix (reservedOp lexer "*" $> Times) AssocLeft]
      ,  [Infix (reservedOp lexer "+" $> Plus) AssocLeft]
      ]

valParser :: Parsec String () PVal
valParser = buildExpressionParser valTable simpleValParser
  where
    simpleValParser =
      (reservedOp lexer "()" $> Unit)
      <|>
      (try (do fname <- funcParser
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
      [ [ Prefix (reserved lexer "inL" $> LeftE)
        , Prefix (reserved lexer "inR" $> RightE)
        ]
      , [ Infix (reservedOp lexer "," $> Pair) AssocLeft ]
      ]

valParserWithLabel :: Parsec String () (Maybe LName, PVal)
valParserWithLabel = do
  label <- option Nothing $ do
    try $ do
      l <- identifier lexer
      symbol lexer "$"
      return (Just l)
  val <- valParser
  return (label, val)

itypParser :: Parsec String () ITyp
itypParser = try parse <|> parens lexer parse
  where
    parse = do
      t1 <- typParser
      symbol lexer "="
      t2 <- typParser
      return (ITyp t1 t2)

startsWithUpper :: String -> Bool
startsWithUpper s = Char.isUpper (s !! 0)

uppercaseParse :: String -> Parsec String () String
uppercaseParse msg = try $ do
  name <- identifier lexer
  if startsWithUpper name
    then return name
    else fail msg

lowercaseParse :: String -> Parsec String () String
lowercaseParse msg = try $ do
  name <- identifier lexer
  if startsWithUpper name
    then fail msg
    else return name


