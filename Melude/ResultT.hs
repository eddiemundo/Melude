{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
module Melude.ResultT where

import Prelude hiding (error)
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import GHC.Stack (HasCallStack)
import Control.Applicative (Applicative(liftA2))
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Monad.Reader (MonadReader(ask, local), ReaderT)
import qualified Melude.Result as Result
import Melude.Result (Result(Result, Errors))
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn)
import Control.Category ((>>>))
import Control.Monad.Trans.Control (MonadTransControl)
import Control.Monad.Identity (IdentityT)
import Control.Monad.Base (MonadBase(liftBase))

-- Notes:
--   If we want MonadBaseControl then write a MonadTransControl instance.
--   Consider not having Result and implementing it directly in ResultT as a data type
newtype ResultT e m a = ResultT { runResultT :: m (Result e a) }
  deriving Functor

instance Applicative m => Applicative (ResultT e m) where
  pure a = a & Result & pure & ResultT
  (<*>) (ResultT mrf) (ResultT mra) = ResultT $ liftA2 (<*>) mrf mra

instance Monad m => Monad (ResultT e m) where
  (>>=) (ResultT mr) f = ResultT $ mr >>= \case
    Result a -> f a & runResultT
    Errors errors -> Errors errors & pure

instance MonadTrans (ResultT e) where
  lift :: Functor m => m a -> ResultT e m a
  lift ma = ma <&> pure & ResultT

instance MonadIO m => MonadIO (ResultT e m) where
  liftIO = liftIO >>> lift

instance MonadBase b m => MonadBase b (ResultT e m) where
  liftBase = liftBase >>> lift

instance MonadReader r m => MonadReader r (ResultT e m) where
  ask :: ResultT e m r
  ask = lift ask

  local :: (r -> r) -> ResultT e m a -> ResultT e m a
  local f (ResultT mr) = local f mr & ResultT

instance MonadState s m => MonadState s (ResultT e m) where
  get :: ResultT e m s
  get = lift get

  put :: s -> ResultT e m ()
  put s = put s & lift

  state :: (s -> (a, s)) -> ResultT e m a 
  state f = state f & lift

class Monad m => MonadResult e m | m -> e where
  errWithCallStack :: HasCallStack => e -> m a
  err :: e -> m a

instance Monad m => MonadResult e (ResultT e m) where
  errWithCallStack :: HasCallStack => e -> ResultT e m a
  errWithCallStack e = Result.errWithCallStack e & pure & ResultT

  err :: e -> ResultT e m a
  err e = Result.err e & pure & ResultT

toErrors :: (Functor m) => ResultT e m a -> m (Seq e)
toErrors (ResultT mr) = mr <&> Result.toErrors 

containsError :: (Functor m, Eq e) => e -> ResultT e m a -> m Bool
containsError error result = toErrors result <&> elem error

mapErrors :: Functor m => (e1 -> e2) -> ResultT e1 m a -> ResultT e2 m a
mapErrors f (ResultT mr) = mr <&> Result.mapErrors f & ResultT

fromMaybe :: (Applicative m) => ResultT e m a -> Maybe a -> ResultT e m a
fromMaybe r Nothing = r
fromMaybe _ (Just a) = pure a

fromEither :: Monad m => (l -> e) -> Either l r -> ResultT e m r
fromEither f (Left l) = f l & err
fromEither _ (Right r) = pure r

fromResult :: (Applicative m) => Result e a -> ResultT e m a
fromResult result = result & pure & ResultT

errorsToText :: (Functor m, Show e) => ResultT e m a -> m Text
errorsToText (ResultT mr) = mr <&> Result.errorsToText

toText :: (Functor m, Show e, Show a) => ResultT e m a -> m Text 
toText (ResultT mr) = mr <&> Result.toText

printErrorsToStdout :: (MonadIO m, Show e) => ResultT e m a -> m ()
printErrorsToStdout r = errorsToText r >>= (Text.putStrLn >>> liftIO)

printToStdout :: (MonadIO m, Show e, Show a) => ResultT e m a -> m ()
printToStdout r = toText r >>= (Text.putStrLn >>> liftIO)

newtype WrappedMonadTrans (t :: (* -> *) -> * -> *) (m :: * -> *) (a :: *)
  = WrappedMonadTrans { unWrappedMonadTrans :: t m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadTransControl)

instance (MonadTransControl t, Monad (t m), MonadResult e m) => MonadResult e (WrappedMonadTrans t m) where
  errWithCallStack = lift . errWithCallStack
  err = lift . err

deriving via (WrappedMonadTrans IdentityT m) instance MonadResult e m => MonadResult e (IdentityT m)
deriving via (WrappedMonadTrans (ReaderT r) m) instance MonadResult e m => MonadResult e (ReaderT r m)
deriving via (WrappedMonadTrans (Lazy.StateT s) m) instance MonadResult e m => MonadResult e (Lazy.StateT s m)
deriving via (WrappedMonadTrans (Strict.StateT s) m) instance MonadResult e m => MonadResult e (Strict.StateT s m)
