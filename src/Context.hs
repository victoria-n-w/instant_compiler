module Context where

import Control.Monad.Reader
import Data.Map
import Instant.ErrM

type Bindings = Map String String

type Context = ReaderT Bindings Err String
