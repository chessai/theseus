module Theseus
  ( main
  )
  where

import Theseus.Repl
import System.IO (hFlush, getLine, stdout)

main :: IO ()
main = repl

replRead :: IO String
replRead = do
  putStr "> "
  hFlush stdout
  getLine

repl :: IO ()
repl = do
  input <- replRead
  case parseCommand (words input) of
    Load file -> do
      run file
      repl
    BadCmd -> do
      putStrLn "Bad command!"
      repl


data Command = Load FilePath | BadCmd

parseCommand :: [String] -> Command
parseCommand ["load", f] = Load f
parseCommand _ = BadCmd
