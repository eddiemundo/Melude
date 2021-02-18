module Main where

import Prelude hiding (error, fail)
import GHC.Stack (HasCallStack)
import Melude.ValidateT (MonadResult (errWithCallStack), runValidateT)
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

getConstraint :: (HasCallStack, MonadState [Int] m, MonadResult Error m) => m Constraint
getConstraint = do
  int <- get >>= \case
    h : t -> put t >> pure h
    _ -> errWithCallStack ConstraintNotFoundError
  Constraint int & pure

getName :: (HasCallStack, MonadState [Int] m, MonadResult Error m) => m Name
getName = do
  int <- get >>= \case
    h : t -> put t >> pure h
    _ -> errWithCallStack NameNotFoundError
  Name int & pure

data Exodia = Exodia Constraint Name deriving Show

makeExodiaM :: (HasCallStack, Applicative m, MonadState [Int] m, MonadResult Error m) => m Exodia
makeExodiaM = do
  c <- getConstraint
  n <- getName
  Exodia c n & pure

makeExodiaA :: (HasCallStack, Applicative m, MonadState [Int] m, MonadResult Error m) => m Exodia
makeExodiaA = Exodia <$> getConstraint <*> getName

main :: IO ()
main = do
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
