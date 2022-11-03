module Main where

import Instant.Abs
import Instant.ErrM
import Instant.Par
import Jvm qualified
import Llvm qualified
import System.Directory.Internal.Prelude (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

process :: String -> Lang -> Err String
process source lang = do
  program <- pProgram $ myLexer source
  case lang of
    JVM -> Jvm.compile program
    LLVM -> Llvm.compile program

data Lang = JVM | LLVM

main :: IO ()
main = do
  args <- getArgs
  let lang = case args of
        ["jvm"] -> JVM
        ["llvm"] -> LLVM
  source <- getContents
  case process source lang of
    Ok res ->
      putStrLn res
    Bad msg -> do
      hPutStrLn stderr $ "Error: " ++ msg
      exitFailure
