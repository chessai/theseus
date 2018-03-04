{-# OPTIONS_GHC -Wall #-}

module Theseus.Repl where

import Theseus.AbstractSyntax
import Theseus.Coverage
import Theseus.Imports
import Theseus.Parser
import Theseus.PrettyPrint
import Theseus.Semantics

import Text.Parsec
import Prelude hiding (init)

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
      print_each [] = pure () 
      print_each ((func, val, res):rs) = 
          do putStr $ "eval " ++ show (ppFunc func) ++ " " ++ show (ppVal val) 
                        ++ " = " ++ show (curVal res) ++ "\n"
             print_each rs
