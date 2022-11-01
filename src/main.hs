module Main where

import Compiler
import Instant.Abs
import Instant.ErrM
import Instant.Par
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

process :: String -> Err String
process source = do
  program <- pProgram $ myLexer source
  compile program

main :: IO ()
main = do
  source <- getContents
  case process source of
    Ok res ->
      putStrLn res
    Bad msg -> do
      hPutStrLn stderr $ "Error: " ++ msg
      exitFailure
