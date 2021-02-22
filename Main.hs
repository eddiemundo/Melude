module Main where

import Prelude hiding (error, fail)
import GHC.Stack (HasCallStack)
import Melude.ValidateT (MonadValidate (errWithCallStack, err, correct, orA), runValidateT, orM, tolerate)
import Control.Monad.State.Strict as Strict
import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Melude.StateT.Strict (evaluateStateT)

data Error
  = PropertyNotFoundError
  | ConstraintNotFoundError
  | NameNotFoundError
  deriving (Eq, Show)

newtype Constraint = Constraint Int deriving Show
newtype Name = Name Int deriving Show

getConstraint :: (HasCallStack, MonadState [Int] m, MonadValidate Error m) => m Constraint
getConstraint = do
  int <- get >>= \case
    h : t -> put t >> pure h
    _ -> errWithCallStack ConstraintNotFoundError
  Constraint int & pure

getName :: (HasCallStack, MonadState [Int] m, MonadValidate Error m) => m Name
getName = do
  int <- get >>= \case
    h : t -> put t >> pure h
    _ -> errWithCallStack NameNotFoundError
  Name int & pure

data Exodia = Exodia Constraint Name deriving Show

makeExodiaM :: (HasCallStack, Applicative m, MonadState [Int] m, MonadValidate Error m) => m Exodia
makeExodiaM = do
  c <- getConstraint
  n <- getName
  Exodia c n & pure

makeExodiaA :: (HasCallStack, Applicative m, MonadState [Int] m, MonadValidate Error m) => m Exodia
makeExodiaA = Exodia <$> getConstraint <*> getName

getNameFail :: (MonadState Int m, MonadValidate Error m) => m Name
getNameFail = err NameNotFoundError <* put 1

getNameSuccess :: (MonadState Int m, MonadValidate Error m) => m Name
getNameSuccess = (Name 1 & pure) <* put 2

getNameSuccessFail :: (MonadState Int m, MonadValidate Error m) => m Name
getNameSuccessFail = getNameFail & correct (const getNameSuccess)

getNameFailFail :: (MonadState Int m, MonadValidate Error m) => m Name
getNameFailFail = getNameFail & correct (const getNameFail)

showAsTextThenPrint :: Show a => a -> IO ()
showAsTextThenPrint a = show a & Text.pack & Text.putStrLn

main :: IO ()
main = do
  getNameFail & tolerate & runValidateT & evaluateStateT 0 >>= showAsTextThenPrint

  getNameSuccess `orA` getNameFail 
    & runValidateT
    & evaluateStateT 0
    >>= showAsTextThenPrint

  getNameFail `orA` getNameSuccess
    & runValidateT
    & evaluateStateT 0
    >>= showAsTextThenPrint

  getNameFail `orA` getNameFail
    & runValidateT
    & evaluateStateT 0
    >>= showAsTextThenPrint

  getNameSuccess `orM` getNameFail 
    & runValidateT
    & evaluateStateT 0
    >>= showAsTextThenPrint

  getNameFail `orM` getNameSuccess
    & runValidateT
    & evaluateStateT 0
    >>= showAsTextThenPrint

  getNameFail `orM` getNameFail
    & runValidateT
    & evaluateStateT 0
    >>= showAsTextThenPrint

  getNameSuccessFail
    & runValidateT
    & evaluateStateT 0 --flip runStateT 0
    >>= showAsTextThenPrint

  getNameSuccessFail
    & flip runStateT 0
    & runValidateT
    >>= showAsTextThenPrint

  getNameFailFail
    & runValidateT
    & flip runStateT 0
    >>= showAsTextThenPrint

  getNameFailFail
    & flip runStateT 0
    & runValidateT
    >>= showAsTextThenPrint

  makeExodiaA
    & flip Strict.runStateT [1,2] 
    & runValidateT
    >>= showAsTextThenPrint

  makeExodiaA
    & runValidateT
    & flip Strict.runStateT [1,2] 
    >>= showAsTextThenPrint

  makeExodiaA
    & runValidateT
    & flip Strict.runStateT []
    >>= showAsTextThenPrint 

  makeExodiaA
    & flip Strict.runStateT []
    & runValidateT
    >>= showAsTextThenPrint 

  makeExodiaM
    & flip Strict.runStateT [1,2] 
    & runValidateT
    >>= showAsTextThenPrint

  makeExodiaM
    & runValidateT
    & flip Strict.runStateT [1,2] 
    >>= showAsTextThenPrint

  makeExodiaM
    & runValidateT
    & flip Strict.runStateT []
    >>= showAsTextThenPrint 

  makeExodiaM
    & flip Strict.runStateT []
    & runValidateT
    >>= showAsTextThenPrint 
