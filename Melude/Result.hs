{-# LANGUAGE FunctionalDependencies #-}
module Melude.Result where

import Prelude hiding (fail, error)
import GHC.Stack (callStack, CallStack, HasCallStack, prettyCallStack)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Function ((&))
import Data.Foldable (fold)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text (Text)
import Data.Functor ((<&>))
import Control.Category ((>>>))

data Error e = Error !(Maybe CallStack) !e deriving Functor

data Result e a
  = Errors !(Seq (Error e))
  | Result a
  deriving Functor

pattern Empty :: Result e a
pattern Empty = Errors Seq.Empty

pattern Cons :: e -> Seq e -> Result e a
pattern Cons error remainingErrors <- (toErrors -> error Seq.:<| remainingErrors)

pattern Snoc :: Seq e -> e -> Result e a
pattern Snoc remainingErrors error <- (toErrors -> remainingErrors Seq.:|> error)

toErrors :: Result e a -> Seq e
toErrors (Errors errors) = fmap (\(Error _ e) -> e) errors
toErrors _ = Seq.empty

containsError :: Eq e => e -> Result e a -> Bool
containsError error result
  | elem error errors = True
  | otherwise = False
  where
    errors = toErrors result

{-# COMPLETE Empty, Cons #-}
{-# COMPLETE Empty, Snoc #-}

instance Applicative (Result e) where
  pure = Result
  (<*>) (Errors leftErrors) (Errors rightErrors) = Errors (leftErrors <> rightErrors)
  (<*>) (Errors errors) _ = Errors errors
  (<*>) _ (Errors errors) = Errors errors
  (<*>) (Result f) (Result a) = Result (f a)

instance Monad (Result e) where
  (>>=) (Errors errors) _ = Errors errors
  (>>=) (Result a) f = f a

errWithCallStack :: HasCallStack => e -> Result e a
errWithCallStack e = Errors (Seq.singleton (Error (Just callStack) e))

err :: e -> Result e a
err e = Errors (Seq.singleton (Error Nothing e))

mapErrors :: (e -> e') -> Result e a -> Result e' a
mapErrors f (Errors errors) = Errors ((fmap . fmap) f errors)
mapErrors _ (Result a) = Result a

fromMaybe :: Result e a -> Maybe a -> Result e a
fromMaybe result Nothing = result
fromMaybe _ (Just a) = Result a

fromEither :: (l -> Result e r) -> Either l r -> Result e r
fromEither f (Left l) = f l
fromEither _ (Right r) = Result r

errorToText :: Show e => Error e -> Text
errorToText (Error (Just stack) e) = Text.pack (show e) <> "\n" <> Text.pack (prettyCallStack stack)
errorToText (Error Nothing e) = Text.pack (show e)

errorSequenceToText :: Show e => Seq (Error e) -> Text
errorSequenceToText errors = errors <&> errorToText & Seq.intersperse "\n" & fold

errorsToText :: Show e => Result e a -> Text
errorsToText = \case
  Errors errors -> errors & errorSequenceToText
  Result _ -> "No errors found."

toText :: (Show e, Show a) => Result e a -> Text
toText r = "Result(" <> bodyToText r <> ")"
  where
    bodyToText = \case
      Errors errors -> errors & errorSequenceToText
      Result a -> show a & Text.pack 

printErrorsToStdout :: Show e => Result e a -> IO ()
printErrorsToStdout (Result _) = pure ()
printErrorsToStdout (Errors errors) = errors & errorSequenceToText & Text.putStrLn

printToStdout :: (Show e, Show a) => Result e a -> IO ()
printToStdout = toText >>> Text.putStrLn

