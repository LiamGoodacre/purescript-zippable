module Data.Zippable
  ( class Zippable
  , zippedWith
  , zipped
  , zipWith
  , zip
  ) where

import Control.Category ((<<<), id)
import Data.Filterable (class Filterable, filtered)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.These (These(..))

-- | `Zippable` represents data structures which support being _zipped_ together.
-- |
-- | - `zippedWith` - TODO
-- |
-- | Laws:
-- | - TODO
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - TODO
class Zippable f where
  zippedWith :: forall x y o.
    (These x y -> o) -> f x -> f y -> f o

zipped :: forall f x y. Zippable f =>
  f x -> f y -> f (These x y)
zipped = zippedWith id

zipWith :: forall f x y o. (Zippable f, Filterable f) =>
  (x -> y -> o) -> f x -> f y -> f o
zipWith f xs ys = filtered (zippedWith applyOnBoth xs ys) where  
  applyOnBoth (This x) = Nothing
  applyOnBoth (That y) = Nothing
  applyOnBoth (Both x y) = Just (f x y)

zip :: forall f x y. (Filterable f, Zippable f) =>
  f x -> f y -> f (Tuple x y)
zip = zipWith Tuple

