module Main where

import Prelude hiding (error, fail)
import GHC.Stack (HasCallStack)
import Melude.ValidateT (MonadValidate (errWithCallStack, err, orElse), runValidateT)
-- import qualified Melude.ResultT as ResultT
import Control.Monad.State.Strict as Strict
import Data.Function ((&))
-- import qualified Melude.ResultT as ResultT
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

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

(<<) :: Monad m => m a -> m b -> m a
(<<) lma rma = do
  a <- lma
  _ <- rma
  pure a

getNameSuccessFail :: (MonadState Int m, MonadValidate Error m) => m Name
getNameSuccessFail = getNameFail & orElse (const getNameSuccess)

getNameFailFail :: (MonadState Int m, MonadValidate Error m) => m Name
getNameFailFail = getNameFail & orElse (const getNameFail)
main :: IO ()
main = do
  getNameSuccessFail
    & runValidateT
    & flip runStateT 0
    >>= (\output -> show output & Text.pack & Text.putStrLn)

  getNameSuccessFail
    & flip runStateT 0
    & runValidateT
    >>= (\output -> show output & Text.pack & Text.putStrLn)

  getNameFailFail
    & runValidateT
    & flip runStateT 0
    >>= (\output -> show output & Text.pack & Text.putStrLn)

  getNameFailFail
    & flip runStateT 0
    & runValidateT
    >>= (\output -> show output & Text.pack & Text.putStrLn)

  makeExodiaA
    & flip Strict.runStateT [1,2] 
    & runValidateT
    >>= (\output -> show output & Text.pack & Text.putStrLn)

  makeExodiaA
    & runValidateT
    & flip Strict.runStateT [1,2] 
    >>= (\output -> show output & Text.pack & Text.putStrLn)

  makeExodiaA
    & runValidateT
    & flip Strict.runStateT []
    >>= (\output -> show output & Text.pack & Text.putStrLn) 

  makeExodiaA
    & flip Strict.runStateT []
    & runValidateT
    >>= (\output -> show output & Text.pack & Text.putStrLn) 

  makeExodiaM
    & flip Strict.runStateT [1,2] 
    & runValidateT
    >>= (\output -> show output & Text.pack & Text.putStrLn)

  makeExodiaM
    & runValidateT
    & flip Strict.runStateT [1,2] 
    >>= (\output -> show output & Text.pack & Text.putStrLn)

  makeExodiaM
    & runValidateT
    & flip Strict.runStateT []
    >>= (\output -> show output & Text.pack & Text.putStrLn) 

  makeExodiaM
    & flip Strict.runStateT []
    & runValidateT
    >>= (\output -> show output & Text.pack & Text.putStrLn) 
