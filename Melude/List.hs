module Melude.List
  ( head
  , last
  , toEither
  , module BaseList
  ) where

import Prelude hiding (head, last)
import Data.List as BaseList hiding (head, last)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

head :: [a] -> Maybe a
head (h : _) = Just h
head _ = Nothing

last :: [a] -> Maybe a
last [] = Nothing
last [h] = Just h
last (_ : t) = last t

toEither :: l -> [r] -> Either l (NonEmpty r)
toEither l rs = 
  case NonEmpty.nonEmpty rs of
    Nothing -> Left l
    Just rs -> Right rs

