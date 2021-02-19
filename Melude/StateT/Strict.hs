module Melude.StateT.Strict 
  ( module Base
  , evaluateStateT
  ) where

import Control.Monad.State.Strict as Base

evaluateStateT :: Monad m => s -> StateT s m a -> m a
evaluateStateT = flip evalStateT
