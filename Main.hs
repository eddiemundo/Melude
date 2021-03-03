module Main where

import Prelude hiding (error, fail)
import GHC.Stack (HasCallStack)
import Melude.ValidateT (MonadValidate, err, runValidateT, correct, orM, orA)
import Control.Monad.State.Strict as Strict
import Data.Function ((&))
import Prettyprinter (Doc, Pretty(pretty), (<+>), vsep, align)
import qualified Prettyprinter.Util as Pretty
import Melude.NonEmptySeq (NonEmptySeq)
import Data.Foldable (Foldable(toList))
import Data.Functor ((<&>))
import Control.Monad.Identity (Identity(runIdentity))

data Error
  = PropertyNotFoundError
  | ConstraintNotFoundError
  | NameNotFoundError
  deriving (Eq, Show)

instance Pretty Error where
  pretty PropertyNotFoundError = "PropertyNotFoundError"
  pretty ConstraintNotFoundError = "ConstraintNotFoundError"
  pretty NameNotFoundError = "NameNotFoundError"

newtype Constraint = Constraint Int deriving (Show, Pretty)
newtype Name = Name Int deriving (Show, Pretty)

getConstraint :: (HasCallStack, MonadState [Int] m, MonadValidate Error m) => m Constraint
getConstraint = do
  int <- get >>= \case
    h : t -> put t >> pure h
    _ -> err ConstraintNotFoundError
  Constraint int & pure

getName :: (HasCallStack, MonadState [Int] m, MonadValidate Error m) => m Name
getName = do
  int <- get >>= \case
    h : t -> put t >> pure h
    _ -> err NameNotFoundError
  Name int & pure

data Exodia = Exodia Constraint Name deriving Show

makeExodiaM :: (HasCallStack, Applicative m, MonadState [Int] m, MonadValidate Error m) => m Exodia
makeExodiaM = do
  c <- getConstraint
  n <- getName
  Exodia c n & pure

makeExodiaA :: (HasCallStack, Applicative m, MonadState [Int] m, MonadValidate Error m) => m Exodia
makeExodiaA = Exodia <$> getConstraint <*> getName

getNameFail :: (HasCallStack, MonadState Int m, MonadValidate Error m) => m Name
getNameFail = err NameNotFoundError <* put 1

getNameSuccess :: (HasCallStack, MonadState Int m, MonadValidate Error m) => m Name
getNameSuccess = (Name 1 & pure) <* put 2

getNameFailCorrectSuccess :: (HasCallStack, MonadState Int m, MonadValidate Error m) => m Name
getNameFailCorrectSuccess = getNameFail & correct (const getNameSuccess)

getNameFailCorrectFail :: (HasCallStack, MonadState Int m, MonadValidate Error m) => m Name
getNameFailCorrectFail = getNameFail & correct (const getNameFail)

instance (Pretty l, Pretty r) => Pretty (Either l r) where
  pretty e = either pretty pretty e

instance Pretty a => Pretty (NonEmptySeq a) where
  pretty nes = nes & toList <&> pretty & vsep & align

instance Pretty Exodia where
  pretty (Exodia constraint name) = "Exodia" <+> pretty constraint <+> pretty name

prettyPrint :: Pretty a => a -> IO ()
prettyPrint result = pretty result & Pretty.putDocW 80

makeDoc :: a -> Doc a
makeDoc = undefined

resultDoc :: HasCallStack => Doc ann
resultDoc = 
  [ getNameSuccess `orA` getNameFail 
    & runValidateT
    & flip runStateT 0
    & runIdentity
    & pretty
  , getNameFail `orA` getNameSuccess
    & runValidateT
    & flip runStateT 0
    & runIdentity
    & pretty
  , getNameFail `orA` getNameFail
    & runValidateT
    & flip runStateT 0
    & runIdentity
    & pretty
  , getNameSuccess `orM` getNameFail 
    & runValidateT
    & flip runStateT 0
    & runIdentity
    & pretty
  , getNameFail `orM` getNameSuccess
    & runValidateT
    & flip runStateT 0
    & runIdentity
    & pretty
  , getNameFail `orM` getNameFail
    & runValidateT
    & flip runStateT 0
    & runIdentity
    & pretty
  , getNameFailCorrectSuccess
    & runValidateT
    & flip runStateT 0
    & runIdentity
    & pretty
  , getNameFailCorrectSuccess
    & flip runStateT 0
    & runValidateT
    & runIdentity
    & pretty
  , getNameFailCorrectFail 
    & runValidateT
    & flip runStateT 0
    & runIdentity
    & pretty
  , getNameFailCorrectFail 
    & flip runStateT 0
    & runValidateT
    & runIdentity
    & pretty
  , makeExodiaA
    & flip runStateT [1,2]
    & runValidateT
    & runIdentity
    & pretty
  , makeExodiaA
    & runValidateT
    & flip runStateT [1,2]
    & runIdentity
    & pretty
  , makeExodiaA
    & runValidateT
    & flip runStateT []
    & runIdentity
    & pretty
    -- note by running state first we lost applicative effect of ValidateT
  , makeExodiaA
    & flip runStateT []
    & runValidateT
    & runIdentity
    & pretty
  , makeExodiaM
    & flip runStateT [1,2]
    & runValidateT
    & runIdentity
    & pretty
  , makeExodiaM
    & runValidateT
    & flip runStateT [1,2]
    & runIdentity
    & pretty
  , makeExodiaM
    & runValidateT
    & flip runStateT []
    & runIdentity
    & pretty
  , makeExodiaM
    & flip runStateT []
    & runValidateT
    & runIdentity
    & pretty
  ]
  & vsep
  & align

main :: IO ()
main = do
  resultDoc & Pretty.putDocW 80


