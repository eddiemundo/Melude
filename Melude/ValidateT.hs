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
  , Internal.WrappedMonadTrans
  ) where

import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import GHC.Stack (HasCallStack, callStack, CallStack, prettyCallStack)
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Trans.Control (MonadTransControl (liftWith, restoreT), MonadBaseControl)
import Control.Monad.Identity (IdentityT)
import Control.Monad.Base (MonadBase)
import Data.Functor.Identity (Identity)
import qualified Data.Sequence.NonEmpty as NonEmptySeq

-- monad-validate
import qualified Control.Monad.Validate as Internal
import qualified Control.Monad.Validate.Class as Internal
import qualified Control.Monad.Validate.Class ()
import qualified Control.Monad.Validate.Internal as Internal

-- prettyprinter
import Prettyprinter (Pretty(pretty), vsep)

import Melude.NonEmptySeq (NonEmptySeq)

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

class Internal.MonadValidate (Failures e) m => MonadValidate e m | m -> e where
  materialize :: m a -> m (Result e a)

instance Monad m => MonadValidate e (ValidateT e m) where
  materialize :: ValidateT e m a -> ValidateT e m (Result e a)
  materialize (ValidateT m) = lift $ Internal.runValidateT m  

materializeAsMaybe :: MonadValidate e m => m a -> m (Maybe a)
materializeAsMaybe = Internal.tolerate

dematerialize :: MonadValidate e m => m (Result e a) -> m a
dematerialize m = m >>= \case
  Left failures -> Internal.refute failures
  Right a -> pure a

err :: (HasCallStack, MonadValidate e m) => e -> m a
err e = Internal.refute $ NonEmptySeq.singleton (Failure callStack e)

errs :: (HasCallStack, MonadValidate e m) => NonEmptySeq e -> m a
errs es = Internal.refute $ es <&> Failure callStack

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
orA ma1 ma2 = correct (\failures1 -> correct (\failures2 -> Internal.refute (failures1 <> failures2)) ma2) ma1

-- toErrors :: (Functor m) => ValidateT e m a -> m (Seq e)
-- toErrors (ValidateT mr) = mr <&> \case
--   Failures failures -> failures <&> (\(Failure _ e) -> e) & NonEmptySeq.toSeq
--   _ -> Seq.empty

-- containsError :: (Functor m, Eq e) => e -> ValidateT e m a -> m Bool
-- containsError error result = toErrors result <&> elem error

-- mapErrors :: Functor m => (e1 -> e2) -> ValidateT e1 m a -> ValidateT e2 m a
-- mapErrors f (ValidateT mr) = mr
--   <&> \case
--     Failures errors -> Failures ((fmap . fmap) f errors) 
--     Success a -> Success a
--   & ValidateT

-- newtype WrappedMonadTrans (t :: (* -> *) -> * -> *) (m :: * -> *) (a :: *)
--   = WrappedMonadTrans { unWrappedMonadTrans :: t m a }
--   deriving (Functor, Applicative, Monad, MonadTrans, MonadTransControl)

-- deriving via (Internal.WrappedMonadTrans (WrappedMonadTrans t) m) 
--   instance Internal.MonadValidate e m => Internal.MonadValidate e (WrappedMonadTrans t m)

instance (MonadTransControl t, Monad (t m), MonadValidate e m) => MonadValidate e (Internal.WrappedMonadTrans t m) where
  materialize :: Internal.WrappedMonadTrans t m a -> Internal.WrappedMonadTrans t m (Result e a)
  materialize wmt1 = liftWith (\run -> materialize (run wmt1)) >>= either (pure . Left) (fmap Right . restoreT . pure)

-- deriving via (WrappedMonadTrans IdentityT m) instance Internal.MonadValidate e m => Internal.MonadValidate e (IdentityT m)

deriving via (Internal.WrappedMonadTrans IdentityT m) instance MonadValidate e m => MonadValidate e (IdentityT m)
deriving via (Internal.WrappedMonadTrans (ReaderT r) m) instance MonadValidate e m => MonadValidate e (ReaderT r m)
deriving via (Internal.WrappedMonadTrans (Lazy.StateT s) m) instance MonadValidate e m => MonadValidate e (Lazy.StateT s m)
deriving via (Internal.WrappedMonadTrans (Strict.StateT s) m) instance MonadValidate e m => MonadValidate e (Strict.StateT s m)
