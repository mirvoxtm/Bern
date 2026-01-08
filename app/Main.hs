module Main where

import System.IO (hFlush, hSetBuffering, BufferMode(NoBuffering), stdout, stdin, BufferMode( LineBuffering ))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Language.Ast
import Language.Eval
import Data.Hashtable.Hashtable
import Language.Helpers
import Parsing.Parser
import Text.Megaparsec (errorBundlePretty)

initialTable :: Hashtable String Value
initialTable = emptyHashtable

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin LineBuffering
  args <- getArgs
  case args of
    [filename] -> runFile filename
    []         -> do
      putStrLn "| Welcome to Bern."
      repl initialTable
    _ -> putStrLn "Usage: bern [filename.brn]"

-- Run a file
runFile :: FilePath -> IO ()
runFile path = do
  contents <- readFile path
  case parseBernFile path contents of
    Left err -> putStrLn (errorBundlePretty err) >> exitFailure
    Right cmd -> do
      _ <- interpretCommand cmd initialTable
      return ()

repl :: Hashtable String Value -> IO ()
repl table = do
  putStr "bern>"
  hFlush stdout
  line <- getLine
  -- Skip empty lines
  if null line || all (== ' ') line
    then repl table
    else case parseBern line of
      Left err -> putStrLn (errorBundlePretty err) >> exitFailure
      Right cmd -> do
        newTable <- interpretCommand cmd table
        repl newTable

printEval :: Expression -> IO ()
printEval expr =
  case evaluate expr initialTable of
    Right v  -> print (getValueOnly v)
    Left err -> putStrLn ("Error: " ++ err)