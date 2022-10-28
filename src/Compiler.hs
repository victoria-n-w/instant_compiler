module Compiler where

import Instant.Abs
import Instant.ErrM
import Instant.Par

compile :: Program -> Err String
compile (Prog []) =
  Bad "Empty"
compile (Prog stmts) =
  return "OK"