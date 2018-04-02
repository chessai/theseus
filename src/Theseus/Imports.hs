{-# OPTIONS_GHC -Wall #-}

module Theseus.Imports where

import Theseus.AbstractSyntax
import Theseus.Parser
import Data.Char as Char
import Text.Parsec (runParser)

-- CR rjames: Now this is not exactly right eince all the defs are put
-- inot one big list. THis will cause different things to be availbale
-- in scope. For now I am going to ignore this. 
loadImports :: [Def] -> [String]-> IO [Def]
loadImports [] _ = pure []
loadImports (Import []:_) _ = pure []
loadImports (x@(DataTyp _ _) : rest) files =
  do defs <- loadImports rest files
     pure $ x:defs
loadImports (x@(Iso _ _ _ _ _) : rest) files =
  do defs <- loadImports rest files
     pure $ x:defs
loadImports (x@(Eval _ _) : rest) files =
  do defs <- loadImports rest files
     pure $ x:defs
loadImports ((Import (f:fs)) : rest) files =
  do let filename = ((Char.toLower f) : fs) ++ ".ths"
     if elem filename files
     then do defs <- loadImports rest files           
             pure $ defs
     else 
       do putStr $ "-- {Loading " ++ filename ++ "}\n";
          input <- readFile filename
          case runParser progParser () filename input of
            Left err ->
              do print err; 
                 pure []
            Right newdefs ->
              do defs <- loadImports (newdefs ++ rest) (filename:files)            
                 pure $ defs
