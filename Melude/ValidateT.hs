{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
module Melude.ValidateT 
  ( errorToResult
  , Result
  , ValidateT(..)
  , Validate
  , runValidateT
  , MonadValidate(materialize)
  , materializeAsMaybe
  , dematerialize
  , correct
  , err
  , errs
  , fromJustOrErr
  , fromRightOrErr
  , orA
  , orM
  , mapErrors
  , Internal.WrappedMonadTrans
  ) where

-- base
import Prelude hiding (fail)
import GHC.Stack (HasCallStack, callStack, CallStack, prettyCallStack)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Control.Monad.IO.Class (MonadIO)

-- transformers-base
import Control.Monad.Base (MonadBase)

-- transformers
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Reader (ReaderT)
import Control.Monad.Identity (IdentityT)

-- mtl
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Reader.Class (MonadReader)
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict

-- monad-control
import Control.Monad.Trans.Control (MonadTransControl(liftWith, restoreT), MonadBaseControl)

-- monad-validate
import qualified Control.Monad.Validate as Internal
import qualified Control.Monad.Validate.Class as Internal
import qualified Control.Monad.Validate.Class ()
import qualified Control.Monad.Validate.Internal as Internal

-- prettyprinter
import Prettyprinter (Pretty(pretty), vsep)

-- melude
import Melude.NonEmptySeq (NonEmptySeq)
import qualified Melude.NonEmptySeq as NonEmptySeq
import qualified Melude.Either as Either

data Failure e = Failure !CallStack !e
  deriving Functor

instance Pretty e => Pretty (Failure e) where
  pretty (Failure stack e) = [pretty e, pretty $ prettyCallStack stack] & vsep

type Failures e = NonEmptySeq (Failure e)

type Result e a = Either (Failures e) a

errorToResult :: HasCallStack => e -> Result e a
errorToResult e = Failure callStack e & NonEmptySeq.singleton & Left

newtype ValidateT e m a = ValidateT (Internal.ValidateT (Failures e) m a)
  deriving 
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadTrans
  , MonadTransControl
  , MonadBase b
  , MonadBaseControl b
  , MonadWriter w
  , MonadState s
  , MonadReader r
  , Internal.MonadValidate (Failures e)
  )

type Validate e a = ValidateT e Identity a

runValidateT :: Functor m => ValidateT e m a -> m (Result e a) 
runValidateT (ValidateT internal) = Internal.runValidateT internal

class Monad m => MonadValidate e m | m -> e where
  fail :: Failures e -> m a
  materialize :: m a -> m (Result e a)

instance Monad m => MonadValidate e (ValidateT e m) where
  fail :: Failures e -> ValidateT e m a
  fail failures = Internal.refute failures & ValidateT

  materialize :: ValidateT e m a -> ValidateT e m (Result e a)
  materialize (ValidateT m) = lift $ Internal.runValidateT m  

err :: (HasCallStack, MonadValidate e m) => e -> m a
err e = Failure callStack e & NonEmptySeq.singleton & fail

errs :: (HasCallStack, MonadValidate e m) => NonEmptySeq e -> m a
errs es = es <&> Failure callStack & fail

materializeAsMaybe :: MonadValidate e m => m a -> m (Maybe a)
materializeAsMaybe ma = materialize ma <&> Either.toMaybe

dematerialize :: MonadValidate e m => m (Result e a) -> m a
dematerialize mr = mr >>= \case
  Left failures -> fail failures
  Right a -> pure a

correct :: MonadValidate e m => (NonEmptySeq (Failure e) -> m a) -> m a -> m a
correct f ma = materialize ma >>= \case
  Left failures -> f failures
  Right _ -> ma

fromJustOrErr :: MonadValidate e m => e -> Maybe a -> m a
fromJustOrErr e Nothing = e & err
fromJustOrErr _ (Just a) = a & pure

fromRightOrErr :: MonadValidate e m => (l -> e) -> Either l r -> m r
fromRightOrErr f (Left l) = f l & err
fromRightOrErr _ (Right r) = r & pure

orM :: MonadValidate e m => m a -> m a -> m a
orM ma1 ma2 = correct (const ma2) ma1

orA :: MonadValidate e m => m a -> m a -> m a
orA ma1 ma2 = correct (\failures1 -> correct (\failures2 -> fail (failures1 <> failures2)) ma2) ma1

-- containsError :: (Functor m, Eq e) => e -> ValidateT e m a -> m Bool
-- containsError error result = toErrors result <&> elem error

mapErrors :: Monad m => (e1 -> e2) -> ValidateT e1 m a -> ValidateT e2 m a
mapErrors f (ValidateT v) = 
  v & Internal.mapErrors (\failures -> failures <&> (\failure -> failure <&> f)) & ValidateT

instance (MonadTransControl t, Monad (t m), MonadValidate e m) => MonadValidate e (Internal.WrappedMonadTrans t m) where
  fail :: Failures e -> Internal.WrappedMonadTrans t m a
  fail = lift . fail

  materialize :: Internal.WrappedMonadTrans t m a -> Internal.WrappedMonadTrans t m (Result e a)
  materialize wmt1 = liftWith (\run -> materialize (run wmt1)) >>= either (pure . Left) (fmap Right . restoreT . pure)

deriving via (Internal.WrappedMonadTrans IdentityT m) instance MonadValidate e m => MonadValidate e (IdentityT m)
deriving via (Internal.WrappedMonadTrans (ReaderT r) m) instance MonadValidate e m => MonadValidate e (ReaderT r m)
deriving via (Internal.WrappedMonadTrans (Lazy.StateT s) m) instance MonadValidate e m => MonadValidate e (Lazy.StateT s m)
deriving via (Internal.WrappedMonadTrans (Strict.StateT s) m) instance MonadValidate e m => MonadValidate e (Strict.StateT s m)

