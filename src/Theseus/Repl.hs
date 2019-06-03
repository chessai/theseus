{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}

module Theseus.Repl
  ( repl
  ) where

import System.Console.Repline
import Control.Monad.IO.Class
import Data.List
import System.Exit
import Text.Parsec (runParser)

import Theseus.Coverage
import Theseus.Eval (run)
import Theseus.Parse (progParser)

type Repl a = HaskelineT IO a

data Line = Load FilePath | BadCmd

parseCommand :: [String] -> Line
parseCommand ["load", f] = Load f
parseCommand _ = BadCmd

banner :: Repl String
banner = pure ">>> "

initialiser :: Repl ()
initialiser = liftIO $ putStrLn "Welcome to THCi"

commandF :: String -> Repl ()
commandF input = parseOneLine input

optionsList :: [(String, [String] -> Repl ())]
optionsList =
  [ ("help", help), ("h", help)
  , ("load", load), ("l", load)
  , ("quit", quit), ("q", quit)
  ]

help :: [String] -> Repl ()
help = const (pure ())

load :: [String] -> Repl ()
load cmdStr = case parseCommand cmdStr of
  Load f -> tryAction $ do
    liftIO $ run f
  BadCmd -> do
    liftIO $ putStrLn $ "unknown command"

quit :: [String] -> Repl ()
quit = const $ do
  liftIO $ do
    putStrLn "Exiting TCHi."
    exitSuccess

completer :: WordCompleter IO
completer n = do
  let names = ["kirk", "spock", "mccoy"]
  pure $ filter (isPrefixOf n) names

repl :: IO ()
repl = evalRepl
  banner
  commandF
  optionsList
  (Just ':')
  (Word completer)
  initialiser

parseOneLine :: String -> Repl ()
parseOneLine parseThis = case runParser progParser () "" parseThis of
  Left err -> liftIO $ do
    print err
  Right tcThis -> case tProg tcThis of
    [] -> do
      pure ()
    errs -> do
      liftIO $ reportErrors errs
