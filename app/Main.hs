module Main where

import System.IO (hFlush, hSetBuffering, BufferMode(NoBuffering), stdout, stdin)
import Language.Ast
import Language.Eval
import Data.Hashtable.Hashtable
import Parsing.Parser

initialTable :: Hashtable String Value
initialTable =
  insertHashtable
    (insertHashtable emptyHashtable "x" (Integer 42))
    "flag"
    (Boolean True)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  putStrLn "| Welcome to Bern."
  repl initialTable

repl :: Hashtable String Value -> IO ()
repl table = do
  putStr "|> "
  hFlush stdout
  line <- getLine
  case parseBern line of
    Left err -> putStrLn ("Parse error: " ++ show err) >> repl table
    Right cmd -> do
      newTable <- interpretCommand cmd table
      repl newTable

printEval :: Expression -> IO ()
printEval expr =
  case evaluate expr initialTable of
    Right v  -> print v
    Left err -> putStrLn ("Error: " ++ err)