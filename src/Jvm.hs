module Jvm (compile) where

import Control.Monad.State
import Data.List
import Data.Map
import Instant.Abs
import Instant.ErrM
import Instant.Par
import Text.Printf

compile :: Program -> String -> Err String
compile p className =
  evalStateT
    (transProgram p className)
    $ Env empty 1

type Result = Err [String]

type Bindings = Map String Int

-- bindings, first free variable
data Env = Env Bindings Int

type Context a = StateT Env Err a

failure :: Show a => a -> Context [String]
failure x = fail $ "Undefined case: " ++ show x

transProgram :: Program -> String -> Context String
transProgram (Prog stmts) className = do
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
            className
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
  (Sub, ExpSub exp11 exp12, ExpSub exp21 exp22) -> do
    optres1 <- transNonComm Sub exp11 exp12
    optres2 <- transNonComm Sub exp21 exp22
    optimizeSwaps op optres1 optres2
  (Div, ExpDiv exp11 exp12, ExpDiv exp21 exp22) -> do
    optres1 <- transNonComm Div exp11 exp12
    optres2 <- transNonComm Div exp21 exp22
    optimizeSwaps op optres1 optres2
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

optimizeSwaps :: Op -> OptResNC -> OptResNC -> Context OptResNC
optimizeSwaps op optres1 optres2 = do
  let nSwapsDontSwap = nNoSwapNeeded optres1 + nNoSwapNeeded optres2
  let nSwapsSwap = nSwapNeeded optres1 + nSwapNeeded optres2
  if heightNC optres1 > heightNC optres2
    then
      return $
        OptResNC
          -- we do an unnecessary swap here, swap is needed higher in the tree
          (swapNeededCode optres1 ++ swapNeededCode optres2 ++ ["swap"] ++ [show op])
          -- we don't do any swaps here, no swaps are neither heigher in the tree either
          (noSwapNeededCode optres1 ++ noSwapNeededCode optres2 ++ [show op])
          (heightNC optres1)
          (nSwapsSwap + 1)
          nSwapsDontSwap
    else
      if heightNC optres2 > heightNC optres1
        then
          return $
            OptResNC
              -- we dont do a swap here, it needs to be done higher in a tree instead
              (noSwapNeededCode optres2 ++ noSwapNeededCode optres1 ++ [show op])
              -- we do a swap here, code is correct the way it is, no swap needed higher in the tree
              (swapNeededCode optres2 ++ swapNeededCode optres1 ++ ["swap"] ++ [show op])
              (heightNC optres2)
              nSwapsDontSwap
              (nSwapsSwap + 1)
        else
          if nSwapsDontSwap <= nSwapsSwap
            then
              return $
                OptResNC
                  -- we artificially swap the code, swap is needed higher up
                  (noSwapNeededCode optres2 ++ noSwapNeededCode optres1 ++ [show op])
                  (noSwapNeededCode optres1 ++ noSwapNeededCode optres2 ++ [show op])
                  (heightNC optres1 + 1)
                  nSwapsDontSwap
                  nSwapsDontSwap
            else
              return $
                OptResNC
                  (swapNeededCode optres1 ++ swapNeededCode optres2 ++ [show op])
                  (swapNeededCode optres2 ++ swapNeededCode optres1 ++ [show op])
                  (heightNC optres1 + 1)
                  nSwapsSwap
                  nSwapsSwap

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
