module Jvm (compile) where

import Control.Monad.State
import Data.List (intercalate)
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
    locs <-
      foldM
        ( \acc x -> do
            compiledStmt <- transStmt x
            return (acc ++ compiledStmt)
        )
        []
        stmts
    return $
      intercalate "\n" locs

transStmt :: Stmt -> Context [String]
transStmt x = case x of
  SAss (Ident ident) exp -> do
    optres <- transExp exp
    let code = case optres of
          One (OptResNode code height _) -> code
          Two (OptResNode code _ _) _ -> code
    (Env binds free_var) <- get
    case Data.Map.lookup ident binds of
      Just num -> do
        -- variable declared before
        return $ code ++ [store num]
      Nothing -> do
        -- variable not declared
        put (Env (Data.Map.insert ident free_var binds) (free_var + 1))
        return $ code ++ [store free_var]
  SExp exp -> do
    -- TODO print
    optres <- transExp exp
    case optres of
      One (OptResNode code height _) -> return code
      Two (OptResNode code _ _) _ -> return code

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
transExp (ExpVar ident) =
  fail "Not implemented var"

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
      return $ One $ OptResNode (code2 ++ code1 ++ [show op] ++ swap) (height2 + 1) (nSwaps1 + nSwaps2)

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

-- TODO optimize
literal :: Integer -> String
literal x
  | 0 < x && x <= 3 = "iput_" ++ show x
  | 3 < x = "iput " ++ show x

-- TODO optimize
store :: Int -> String
store x
  | 0 < x && x <= 3 = "store_" ++ show x
  | 3 < x = "store " ++ show x
