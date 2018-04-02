{-# OPTIONS_GHC -Wall #-}

module Theseus.Parser where

import Theseus.AbstractSyntax

import Control.Monad.Identity

import Data.List as List
import Data.Char as Char

import Text.Parsec hiding (label, labels, parse)
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

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
                pure defs

defParser :: Parsec String () Def
defParser = 
    do t <- reserved lexer "data" >> identifier lexer
       _ <- symbol lexer "="
       args <- (sepBy (do ctor <- identifier lexer
                          typ <- option One typParser
                          pure (ctor, typ))
                      (symbol lexer "|"))
       pure (DataTyp t (List.sort args))
    <|>
    (do reserved lexer "iso"
        fname <- try (do fname <- identifier lexer
                         reserved lexer ":"
                         pure fname)
        fparams <- many (try (do arg <- identifier lexer
                                 _ <- symbol lexer ":"
                                 ityp <- itypParser
                                 _ <- symbol lexer "->"
                                 pure (arg, ityp)))
        ityp <- itypParser
        clauses <- many (do _ <- symbol lexer "|" 
                            p1 <- valParserWithLabel 
                            _ <- symbol lexer "="
                            p2 <- valParserWithLabel
                            pure (Clause p1 p2))
        labels <- option [] -- now I need a "where" per label.
                  (do reserved lexer "where"
                      many1 (do label <- identifier lexer
                                _ <- symbol lexer ":" 
                                typ <- typParser
                                pure (label, typ)))
        pure (Iso fname fparams ityp labels clauses))
    <|>
    (do reserved lexer "eval" 
        func <- funcParser 
        v <- valParser 
        pure (Eval func v))
    <|>
    (do reserved lexer "import" 
        modname <- uppercaseParse "module name"
        pure (Import modname))


funcParser :: Parsec String () Func 
funcParser = parser
    where 
      simpleParser = 
          (do name <- lowercaseParse "iso name"
              pure (Func name []))
          <|>
          (parens lexer parser)
      parser = 
          do name <- lowercaseParse "iso name"
             args <- many (do _ <- symbol lexer "~"
                              label <- lowercaseParse "label"
                              opt <- option Nothing 
                                     (do _ <- symbol lexer ":"
                                         arg <- simpleParser
                                         pure (Just arg))
                              pure (label, opt))
             pure (Func name args)

typParser :: Parsec String () Typ 
typParser = buildExpressionParser typTable simpleTypParser
  where simpleTypParser = 
            (symbol lexer "0" >> pure Zero) 
            <|> 
            (symbol lexer "1" >> pure One) 
            <|> 
            (do t <- identifier lexer
                pure (TName t))
            <|>
            (parens lexer typParser)
        typTable = 
            [[Prefix (reservedOp lexer "-" >> pure Neg)],
             [Infix (reservedOp lexer "*" >> pure Times) AssocLeft],
             [Infix (reservedOp lexer "+" >> pure Plus) AssocLeft]]

valParser :: Parsec String () PVal
valParser = buildExpressionParser valTable simpleValParser
    where 
      simpleValParser = 
          (reservedOp lexer "()" >> pure Unit)
          <|>
          (try (do fname <- funcParser; 
                   val <- simpleValParser
                   pure (App fname val)))
          <|>
          (do name <- lowercaseParse "variable"
              pure (Var name)) 
          <|>
          (do name <- uppercaseParse "constructor";
              val <- option Unit simpleValParser
              pure (Constr name val))
          <|>
          (parens lexer valParser)
      valTable = 
          [[Prefix (do _ <- reserved lexer "inL"; pure LeftE),
            Prefix (do _ <- reserved lexer "inR"; pure RightE)],
           [Infix (reservedOp lexer "," >> pure Pair) AssocLeft]]

valParserWithLabel :: Parsec String () (Maybe LName, PVal)
valParserWithLabel = do 
  do label <- option Nothing 
              (try (do l <- identifier lexer
                       _ <- symbol lexer "$"
                       pure (Just l)))
     val <- valParser 
     pure (label, val)

itypParser :: Parsec String () ITyp
itypParser = try parse 
             <|> parens lexer parse
    where 
      parse = typParser >>= \t1 -> 
              symbol lexer "=" >>
              typParser >>= \t2 -> 
              pure (ITyp t1 t2)


startsWithUpper :: String -> Bool
startsWithUpper s = Char.isUpper (s !! 0) 

uppercaseParse :: String -> Parsec String () String
uppercaseParse msg = try (do name <- identifier lexer
                             if startsWithUpper name
                             then pure name
                             else fail msg)

lowercaseParse :: String -> Parsec String () String
lowercaseParse msg = try (do name <- identifier lexer
                             if startsWithUpper name 
                             then fail msg
                             else pure name)
