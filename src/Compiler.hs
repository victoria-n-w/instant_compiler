module Compiler where

import Control.Monad.State
import Data.List (intercalate)
import Data.Map
import Instant.Abs
import Instant.ErrM
import Instant.Par
import Text.Printf

data RetValue = Register Int | Literal Integer

instance Show RetValue where
  show (Register r) = "%r" ++ show r
  show (Literal r) = show r

data LlvmResult = LlvmResult RetValue [String]

type Bindings = Map String String

data Env = Env Bindings Int

type Context a = StateT Env Err a

failure :: Show a => a -> Context [String]
failure x = fail $ show x ++ " NOT IMPLEMENTED"

failureExp :: Show a => a -> Context LlvmResult
failureExp x = fail $ show x ++ " NOT IMPLEMENTED"

transProgram :: Program -> Context String
transProgram x = case x of
  Prog stmts -> do
    locs <-
      foldM
        ( \acc x -> do
            compiledStmt <- transStmt x
            return (acc ++ compiledStmt)
        )
        []
        stmts
    return $ intercalate "\n" locs

transStmt :: Stmt -> Context [String]
transStmt x = case x of
  SAss ident exp -> failure x
  SExp exp -> do
    LlvmResult _ code <- transExp exp
    return code

transExp :: Exp -> Context LlvmResult
transExp x = case x of
  ExpAdd exp1 exp2 -> transBinaryExp "add" exp1 exp2
  ExpSub exp1 exp2 -> transBinaryExp "sub" exp1 exp2
  ExpMul exp1 exp2 -> transBinaryExp "mul" exp1 exp2
  ExpDiv exp1 exp2 -> failureExp x
  ExpLit integer -> return $ LlvmResult (Literal integer) []
  ExpVar ident -> failureExp x

transBinaryExp :: String -> Exp -> Exp -> Context LlvmResult
transBinaryExp op exp1 exp2 = do
  LlvmResult r1 code1 <- transExp exp1
  LlvmResult r2 code2 <- transExp exp2
  r <- getRegister
  return $
    LlvmResult r $
      code1
        ++ code2
        ++ [printf "%s = %s i32 %s, %s" (show r) op (show r1) (show r2)]

getRegister :: Context RetValue
getRegister = do
  Env binds r <- get
  put (Env binds (r + 1))
  return $ Register r

compile :: Program -> Err String
compile p =
  evalStateT
    (transProgram p)
    $ Env empty 0
