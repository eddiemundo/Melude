module Melude.Either 
  ( toMaybe
  , rightOrElse
  , rightOrThrow
  , module BaseEither
  ) where

import Prelude hiding (either)
import Data.Either as BaseEither hiding (either)
import Control.Exception (throw, Exception)
import Control.Arrow ((>>>))

toMaybe :: Either e a -> Maybe a
toMaybe = \case
  Left _ -> Nothing
  Right a -> Just a

rightOrElse :: (l -> r) -> Either l r -> r
rightOrElse handler either =
  case either of
    Left l -> handler l
    Right r -> r

rightOrThrow :: Exception e => (l -> e) -> Either l r -> r
rightOrThrow handler = rightOrElse (handler >>> throw)
