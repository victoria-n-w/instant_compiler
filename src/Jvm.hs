module Jvm (compile) where

import Control.Monad.State
import Data.List
import Data.Map
import Instant.Abs
import Instant.ErrM
import Instant.Par
import Text.Printf

compile :: Program -> Err String
compile p =
  evalStateT
    (transProgram p)
    $ Env empty 1

type Result = Err [String]

type Bindings = Map String Int

-- bindings, first free variable
data Env = Env Bindings Int

type Context a = StateT Env Err a

failure :: Show a => a -> Context [String]
failure x = fail $ "Undefined case: " ++ show x

transProgram :: Program -> Context String
transProgram (Prog stmts) = do
  (locs, height, locals) <-
    foldM
      ( \(accCode, accHeight, accLocal) x -> do
          res <- transStmt x
          case res of
            Declared code height -> return (accCode ++ code, max accHeight height, accLocal + 1)
            Res code height -> return (accCode ++ code, max accHeight height, accLocal)
      )
      ([], 0, 0)
      stmts
  return $
    intercalate "\n" $
      Prelude.map
        identJvm
        ( classHeader
            "Name"
            ++ methodHeader height (locals + 1)
            ++ locs
            ++ methodOutro
        )

data StmtRes
  = Declared
      { stmtCode :: [String],
        stackHeight :: Int
      }
  | Res
      { stmtCode :: [String],
        stackHeight :: Int
      }

transStmt :: Stmt -> Context StmtRes
transStmt (SAss (Ident ident) exp) = do
  (OptRes code height) <- transExp exp
  (Env binds freeVar) <- get
  case Data.Map.lookup ident binds of
    Just num -> do
      -- variable declared before
      return $ Res (code ++ [store num]) height
    Nothing -> do
      -- variable not declared
      put (Env (Data.Map.insert ident freeVar binds) (freeVar + 1))
      return $ Declared (code ++ [store freeVar]) height
transStmt (SExp exp) = do
  (OptRes code height) <- transExp exp
  if height > 1
    then return $ Res (code ++ getPrintStream ++ ["swap"] ++ printInt) height
    else return $ Res (getPrintStream ++ code ++ printInt) 2

transExp :: Exp -> Context OptRes
transExp (ExpAdd exp1 exp2) = transComm Add exp1 exp2
transExp (ExpMul exp1 exp2) = transComm Mul exp1 exp2
transExp (ExpSub exp1 exp2) = do
  optres <- transNonComm Sub exp1 exp2
  return $ OptRes (noSwapNeededCode optres) (heightNC optres)
transExp (ExpDiv exp1 exp2) = do
  optres <- transNonComm Div exp1 exp2
  return $ OptRes (noSwapNeededCode optres) (heightNC optres)
transExp (ExpLit int) = return $ OptRes [literal int] 1
transExp (ExpVar (Ident ident)) = do
  (Env binds _) <- get
  case Data.Map.lookup ident binds of
    Nothing -> fail $ "Variable not declared: " ++ ident
    Just a ->
      return $ OptRes [load a] 1

data OptRes = OptRes
  { code :: [String],
    height :: Int
  }

transComm :: Op -> Exp -> Exp -> Context OptRes
transComm op exp1 exp2 = do
  OptRes code1 height1 <- transExp exp1
  OptRes code2 height2 <- transExp exp2
  if height1 >= height2
    then -- code 1 first
      return $ OptRes (code1 ++ code2 ++ [show op]) $ max height1 (height2 + 1)
    else do
      return $ OptRes (code2 ++ code1 ++ [show op]) $ max height2 (height1 + 1)

data OptResNC = OptResNC
  { swapNeededCode :: [String],
    noSwapNeededCode :: [String],
    heightNC :: Int,
    nSwapNeeded :: Int,
    nNoSwapNeeded :: Int
  }

transNonComm :: Op -> Exp -> Exp -> Context OptResNC
transNonComm op exp1 exp2 = case (op, exp1, exp2) of
  (Sub, ExpSub _ _, ExpSub _ _) -> fail "not implemented"
  (Div, ExpDiv _ _, ExpDiv _ _) -> fail "not implemented"
  _ -> do
    OptRes code1 height1 <- transExp exp1
    OptRes code2 height2 <- transExp exp2
    if height1 >= height2
      then
        return $
          OptResNC
            -- we need to swap the ancestor if we do a swap here
            (code1 ++ code2 ++ ["swap"] ++ [show op])
            -- we don't need to swap the ancestor, if we don't do a swap here
            (code1 ++ code2 ++ [show op])
            (max height1 (height2 + 1))
            1
            0
      else
        return $
          OptResNC
            -- we need to swap the ancestor if we don't do a swap here
            (code2 ++ code1 ++ [show op])
            -- we don't need to swap the ancestor, if we do a swap here
            (code2 ++ code1 ++ ["swap"] ++ [show op])
            (max height2 (height1 + 1))
            0
            1

data Op
  = Add
  | Mul
  | Sub
  | Div

instance Show Op where
  show Add = "iadd"
  show Mul = "imul"
  show Sub = "isub"
  show Div = "idiv"

isCommutative :: Op -> Bool
isCommutative Add = True
isCommutative Mul = True
isCommutative Sub = False
isCommutative Div = False

literal :: Integer -> String
literal x
  | 0 < x && x <= 5 = "iconst_" ++ show x
  | x <= 127 = "bipush " ++ show x
  | x <= 32767 = "sipush " ++ show x
  | otherwise = "ldc " ++ show x

store :: Int -> String
store x
  | 0 <= x && x <= 3 = "istore_" ++ show x
  | 3 < x = "istore " ++ show x

load :: Int -> String
load x
  | 0 <= x && x <= 3 = "iload_" ++ show x
  | 3 < x = "iload " ++ show x

classHeader :: String -> [String]
classHeader name =
  [ printf ".class public %s" name,
    ".super java/lang/Object",
    ".method public <init>()V",
    "aload_0",
    "invokespecial java/lang/Object/<init>()V",
    "return",
    ".end method"
  ]

methodHeader :: Int -> Int -> [String]
methodHeader stackLimit localLimit =
  [ ".method public static main([Ljava/lang/String;)V",
    printf ".limit stack %d" stackLimit,
    printf ".limit locals %d" localLimit
  ]

methodOutro :: [String]
methodOutro =
  ["return", ".end method"]

getPrintStream = ["getstatic java/lang/System/out Ljava/io/PrintStream;"]

printInt = ["invokevirtual java/io/PrintStream/println(I)V"]

identJvm :: String -> String
identJvm s =
  case s of
    ('.' : _) -> s
    _ -> "\t" ++ s
