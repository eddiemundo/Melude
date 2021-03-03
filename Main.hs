module Main where

import Prelude hiding (error, fail)
import GHC.Stack (HasCallStack)
import Melude.ValidateT (MonadValidate, err, runValidateT, tolerate)
import Control.Monad.State.Strict as Strict
import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Melude.StateT.Strict (evaluateStateT)
import qualified Melude.Result as Result
import qualified Melude.ValidateT as ValidateT
import Prettyprinter (Doc, Pretty(pretty, prettyList), (<+>), vsep, align)
import qualified Prettyprinter.Util as Pretty
import Melude.NonEmptySeq (NonEmptySeq)
import qualified Melude.NonEmptySeq as NonEmptySeq
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

-- instance Pretty Constraint 

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

getNameFail :: (MonadState Int m, MonadValidate Error m) => m Name
getNameFail = err NameNotFoundError <* put 1

getNameSuccess :: (MonadState Int m, MonadValidate Error m) => m Name
getNameSuccess = (Name 1 & pure) <* put 2

-- getNameSuccessFail :: (MonadState Int m, MonadValidate Error m) => m Name
-- getNameSuccessFail = getNameFail & correct (const getNameSuccess)

-- getNameFailFail :: (MonadState Int m, MonadValidate Error m) => m Name
-- getNameFailFail = getNameFail & correct (const getNameFail)

-- printResult :: Show a => Either (ValidateT.Failures e) a -> IO ()
-- printResult (Left failures) = Result.failuresToText failures & Text.putStrLn   -- show a & Text.pack & Text.putStrLn
-- printResult (Right a) = show a & Text.pack & Text.putStrLn   -- show a & Text.pack & Text.putStrLn

-- newtype Result l r = Result (Either l r)

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

resultDoc :: Doc ann
resultDoc = 
  [ makeExodiaA
    & flip Strict.runStateT [1,2]
    & runValidateT
    & runIdentity
    & pretty
  , makeExodiaA
    & runValidateT
    & flip Strict.runStateT [1,2]
    & runIdentity
    & pretty
  , makeExodiaA
    & runValidateT
    & flip Strict.runStateT []
    & runIdentity
    & pretty
  , makeExodiaA
    & flip Strict.runStateT []
    & runValidateT
    & runIdentity
    & pretty
  , makeExodiaM
    & flip Strict.runStateT [1,2]
    & runValidateT
    & runIdentity
    & pretty
  , makeExodiaM
    & runValidateT
    & flip Strict.runStateT [1,2]
    & runIdentity
    & pretty
  , makeExodiaM
    & runValidateT
    & flip Strict.runStateT []
    & runIdentity
    & pretty
  , makeExodiaM
    & flip Strict.runStateT []
    & runValidateT
    & runIdentity
    & pretty
  ]
  & vsep
  & align

main :: IO ()
main = do
  -- getNameFail & tolerate & runValidateT & evaluateStateT 0 >>= prettyPrint

--   getNameSuccess `orA` getNameFail 
--     & runValidateT
--     & evaluateStateT 0
--     >>= showAsTextThenPrint

--   getNameFail `orA` getNameSuccess
--     & runValidateT
--     & evaluateStateT 0
--     >>= showAsTextThenPrint

--   getNameFail `orA` getNameFail
--     & runValidateT
--     & evaluateStateT 0
--     >>= showAsTextThenPrint

--   getNameSuccess `orM` getNameFail 
--     & runValidateT
--     & evaluateStateT 0
--     >>= showAsTextThenPrint

--   getNameFail `orM` getNameSuccess
--     & runValidateT
--     & evaluateStateT 0
--     >>= showAsTextThenPrint

--   getNameFail `orM` getNameFail
--     & runValidateT
--     & evaluateStateT 0
--     >>= showAsTextThenPrint

--   getNameSuccessFail
--     & runValidateT
--     & evaluateStateT 0 --flip runStateT 0
--     >>= showAsTextThenPrint

  -- getNameSuccessFail
  --   & flip runStateT 0
  --   & runValidateT
  --   >>= showAsTextThenPrint

  -- getNameFailFail
  --   & runValidateT
  --   & flip runStateT 0
  --   >>= showAsTextThenPrint

  -- getNameFailFail
  --   & flip runStateT 0
  --   & runValidateT
  --   >>= showAsTextThenPrint

  --   >>= prettyPrint
  resultDoc & Pretty.putDocW 80


