module Main where

import System.IO (hFlush, hSetBuffering, BufferMode(NoBuffering), stdout, stdin, BufferMode( LineBuffering ))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import GHC.IO.Encoding (setLocaleEncoding, utf8)
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
  setLocaleEncoding utf8
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin LineBuffering
  args <- getArgs
  case args of
    ["."]      -> runMainBrn
    ["help"]   -> printHelp
    [filename] -> runFile filename
    []         -> do 
      printWelcome
      putStrLn ""
      repl initialTable
    _ -> putStrLn "Usage: bern [filename.brn | help]"

runMainBrn :: IO ()
runMainBrn = do
  cwd <- getCurrentDirectory
  let mainFile = cwd </> "main.brn"
  exists <- doesFileExist mainFile
  if exists
    then runFile mainFile
    else do
      putStrLn "Error: No main.brn found in current directory."
      exitFailure

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: bern [filename.brn | help]"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  bern              Start the REPL"
  putStrLn "  bern .            Run ./main.brn from the current directory"
  putStrLn "  bern <file.brn>   Run a Bern source file"
  putStrLn "  bern help         Show this message"
      
printWelcome :: IO ()
printWelcome = do
  let purple = "\x1b[35m"
  let reset  = "\x1b[0m"

  putStrLn (purple ++ "______                  " ++ reset)
  putStrLn (purple ++ "| ___ \\                      The Bern Language Interpreter " ++ reset)
  putStrLn (purple ++ "| |_/ / ___ _ __ _ __       " ++ reset)
  putStrLn (purple ++ "| ___ \\/ _ \\ '__| '_ \\     Use \"bern <file>\" to run a program." ++ reset)
  putStrLn (purple ++ "| |_/ /  __/ |  | | | |      Use \"bern help\" for a tutorial." ++ reset)
  putStrLn (purple ++ "\\____/ \\___|_|  |_| |_|         [ v.1.0.1 12.01.2025 ]" ++ reset)


-- Run a file
runFile :: FilePath -> IO ()
runFile path = do
  contents <- readFile path
  case parseBernFile path contents of
    Left err -> putStrLn (errorBundlePretty err) >> exitFailure
    Right cmd -> do
      _ <- interpretCommand Nothing cmd initialTable
      return ()

repl :: Hashtable String Value -> IO ()
repl table = do
  let purple = "\x1b[35m"
      reset  = "\x1b[0m"
  putStr (purple ++ "[bern]: " ++ reset)
  hFlush stdout
  line <- getLine
  -- Skip empty lines
  if null line || all (== ' ') line
    then repl table
    else case parseBern line of
      Left err -> putStrLn (errorBundlePretty err) >> exitFailure
      Right cmd -> do
        newTable <- interpretCommand Nothing cmd table
        repl newTable

printEval :: Expression -> IO ()
printEval expr =
  case evaluate expr initialTable of
    Right v  -> print (getValueOnly v)
    Left err -> putStrLn (formatError err)
