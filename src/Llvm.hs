module Llvm (compile) where

import Control.Monad.State
import Data.IntMap qualified as Data.Map
import Data.List (intercalate)
import Data.Set
import Instant.Abs
import Instant.ErrM
import Instant.Par
import Text.Printf

data RetValue = Register Int | Literal Integer

instance Show RetValue where
  show (Register r) = "%r" ++ show r
  show (Literal r) = show r

data LlvmResult = LlvmResult RetValue [String]

type Bindings = Set String

data Env = Env Bindings Int

type Context a = StateT Env Err a

header =
  [ "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"",
    "declare i32 @printf(i8*, ...)",
    "define void @printInt(i32 %x) {",
    "%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0",
    "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)",
    "ret void",
    "}"
  ]

mainEntry =
  [ "define i32 @main(i32 %argc, i8** %argv) {",
    "entry:"
  ]

mainRet =
  [ "ret i32 0",
    "}"
  ]

transProgram :: Program -> Context String
transProgram (Prog _ stmts) = do
  locs <-
    foldM
      ( \acc x -> do
          compiledStmt <- transStmt x
          return (acc ++ compiledStmt)
      )
      []
      stmts
  return $
    intercalate "\n" $
      header ++ mainEntry ++ locs ++ mainRet

transStmt :: Stmt -> Context [String]
transStmt (SAss _ (Ident ident) exp) = do
  LlvmResult r code <- transExp exp
  Env binds old_r <- get
  if Data.Set.member ident binds
    then
      return $
        code ++ [printf "store i32 %s, i32* %s" (show r) ("%" ++ ident)]
    else -- new variable decalared
    do
      put (Env (Data.Set.insert ident binds) old_r)
      return $
        code
          ++ [ printf "%s = alloca i32" ("%" ++ ident),
               printf "store i32 %s, i32* %s" (show r) ("%" ++ ident)
             ]
transStmt (SExp _ exp) = do
  LlvmResult r code <- transExp exp
  return $ code ++ [printf "call void @printInt(i32 %s)" (show r)]

transExp :: Exp -> Context LlvmResult
transExp (ExpAdd _ exp1 exp2) = transBinaryExp "add" exp2 exp1 -- swap the expressions
transExp (ExpSub _ exp1 exp2) = transBinaryExp "sub" exp1 exp2
transExp (ExpMul _ exp1 exp2) = transBinaryExp "mul" exp1 exp2
transExp (ExpDiv _ exp1 exp2) = transBinaryExp "sdiv" exp1 exp2
transExp (ExpLit _ integer) = return $ LlvmResult (Literal integer) []
transExp (ExpVar (Just position) (Ident ident)) = do
  Env binds _ <- get
  if Data.Set.member ident binds
    then do
      r <- getRegister
      return $
        LlvmResult
          r
          [printf "%s = load i32, i32* %s" (show r) ("%" ++ ident)]
    else fail $ printf "%s: variable [%s] not initialized" (show position) ident

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
