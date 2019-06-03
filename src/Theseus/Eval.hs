{-# language LambdaCase #-}

module Theseus.Eval
  ( run
  ) where

import Data.Char as Char
import Text.Parsec (runParser)

import Theseus.AbstractSyntax
import Theseus.Pretty
import Theseus.Parse
import Theseus.Coverage
import Theseus.Semantics

-- this is not exactly right since all the defs are put
-- into one big list. This will cause different things to be available
-- in scope. For now I am going to ignore this.
loadImports :: [Def] -> [String]-> IO [Def]
loadImports [] _files = return []
loadImports (x@(DataTyp _ _) : rest) files =
  do defs <- loadImports rest files
     return $ x:defs
loadImports (x@(Iso _ _ _ _ _) : rest) files =
  do defs <- loadImports rest files
     return $ x:defs
loadImports (x@(Eval _ _) : rest) files =
  do defs <- loadImports rest files
     return $ x:defs
loadImports (Import [] : rest) _files = pure rest
loadImports (Import (f:fs) : rest) files =
  do let filename = ((Char.toLower f) : fs) ++ ".ths"
     if elem filename files
     then do defs <- loadImports rest files
             return $ defs
     else do
       putStr $ "-- {Loading " ++ filename ++ "}\n";
       input <- readFile filename
       case runParser progParser () filename input of
         Left err -> do
           print err
           return []
         Right newdefs -> do
           defs <- loadImports (newdefs ++ rest) (filename:files)
           return defs

------------------------------------------------------------------------
-- Read, process, print

type Filename = String

repIO :: Filename -> (Prog -> IO ()) -> IO ()
repIO ifile processProg = do
  input <- readFile ifile
  either print processProg (runParser progParser () ifile input)

-- typecheck and evaluate
run :: Filename -> IO ()
run ifile = repIO ifile $ \initial -> do
  p <- loadImports initial [ifile]
  putStr "Typechecking...\n"
  case tProg p of
    [] -> do
      putStr "Evaluating...\n"
      (_fenv, res) <- evalProg p (FEnv []) True
      print_each res
    errs -> reportErrors errs
  where
    print_each = \case
      [] -> return ()
      ((func, val, res):rs) -> do
        putStr $
          "eval " ++ show (ppFunc func) ++ " " ++ show (ppVal val)
          ++ " = " ++ show (curVal res) ++ "\n"
        print_each rs


