module Compiler where

import Context
import Control.Monad.Reader
import Data.Map
import Instant.Abs
import Instant.ErrM
import Instant.Par

failure :: Show a => a -> Context
failure x = fail "NOT IMPLEMENTED"

transIdent :: Ident -> Context
transIdent x = case x of
  Ident string -> failure x

transProgram :: Program -> Context
transProgram x = case x of
  Prog stmts ->
    foldM
      ( \acc x -> do
          compiledStmt <- transStmt x
          return (acc ++ compiledStmt ++ "\n")
      )
      ""
      stmts

transStmt :: Stmt -> Context
transStmt x = case x of
  SAss ident exp -> do return "STMT WITH ID"
  SExp exp -> do return "STMT EXP"

transExp :: Exp -> Context
transExp x = case x of
  ExpAdd exp1 exp2 -> failure x
  ExpSub exp1 exp2 -> failure x
  ExpMul exp1 exp2 -> failure x
  ExpDiv exp1 exp2 -> failure x
  ExpLit integer -> failure x
  ExpVar ident -> failure x

compileStmts :: Program -> Context
compileStmts (Prog []) = return ""

compile :: Program -> Err String
-- empty program, no output
compile (Prog []) = do return "empty??"
compile p = runReaderT (transProgram p) empty
