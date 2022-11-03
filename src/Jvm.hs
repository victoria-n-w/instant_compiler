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
transProgram x = case x of
  Prog stmts -> do
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

-- code, stack height, declared locals
data StmtRes = Declared [String] Int | Res [String] Int

transStmt :: Stmt -> Context StmtRes
transStmt x = case x of
  SAss (Ident ident) exp -> do
    optres <- transExp exp
    let (OptResNode code height _) = getFirst optres
    (Env binds free_var) <- get
    case Data.Map.lookup ident binds of
      Just num -> do
        -- variable declared before
        return $ Res (code ++ [store num]) height
      Nothing -> do
        -- variable not declared
        put (Env (Data.Map.insert ident free_var binds) (free_var + 1))
        return $ Declared (code ++ [store free_var]) height
  SExp exp -> do
    optres <- transExp exp
    let (OptResNode code height _) = getFirst optres
    if height > 1
      then return $ Res (code ++ getPrintStream ++ ["swap"] ++ printInt) height
      else return $ Res (getPrintStream ++ code ++ printInt) 2

data OptResNode = OptResNode [String] Int Int

data OptRes
  = One OptResNode
  | Two OptResNode OptResNode

getFirst :: OptRes -> OptResNode
getFirst (One node) = node
getFirst (Two node _) = node

transExp :: Exp -> Context OptRes
transExp (ExpAdd exp1 exp2) = transOptimizeStack Add exp1 exp2
transExp (ExpMul exp1 exp2) = transOptimizeStack Mul exp1 exp2
transExp (ExpSub exp1 exp2) =
  case (exp1, exp2) of
    (ExpSub _ _, ExpSub _ _) -> transOptimizeStackSwaps Sub exp1 exp2
    _ -> transOptimizeStack Sub exp1 exp2
transExp (ExpDiv exp1 exp2) =
  case (exp1, exp2) of
    (ExpDiv _ _, ExpDiv _ _) -> transOptimizeStackSwaps Div exp1 exp2
    _ -> transOptimizeStack Div exp1 exp2
transExp (ExpLit int) = return $ One $ OptResNode [literal int] 1 0
transExp (ExpVar (Ident ident)) = do
  (Env binds _) <- get
  case Data.Map.lookup ident binds of
    Nothing -> fail $ "Variable not declared: " ++ ident
    Just a ->
      return $ One $ OptResNode [load a] 1 0

transOptimizeStack :: Op -> Exp -> Exp -> Context OptRes
transOptimizeStack op exp1 exp2 = do
  res1 <- transExp exp1
  res2 <- transExp exp2
  let OptResNode code1 height1 nSwaps1 = getFirst res1
  let OptResNode code2 height2 nSwaps2 = getFirst res2
  if height1 > height2
    then -- code 1 first
      return $ One $ OptResNode (code1 ++ code2 ++ [show op]) (height1 + 1) (nSwaps1 + nSwaps2)
    else do
      -- swap
      let swap = ["swap" | not (isCommutative op)]
      return $ One $ OptResNode (code2 ++ code1 ++ swap ++ [show op]) (height2 + 1) (nSwaps1 + nSwaps2)

-- TODO
transOptimizeStackSwaps :: Op -> Exp -> Exp -> Context OptRes
transOptimizeStackSwaps = transOptimizeStack

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
