module Data.Zippable
  ( class Zippable
  , partitionZip
  , filterZip
  , zipWith
  , zip
  ) where

import Data.Filterable (class Filterable)
import Data.Maybe (Maybe())
import Data.Either (Either())
import Data.Tuple (Tuple(..))
import Data.These (These())

-- | `Zippable` represents data structures which support being _zipped_ together.
-- |
-- | - `partitionZip` - TODO
-- | - `filterZip` - TODO
-- | - `zipWith` - TODO
-- |
-- | Laws:
-- | - TODO
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - TODO
class Filterable f <= Zippable f where
  partitionZip :: forall x y l r.
    (These x y -> Either l r) -> f x -> f y -> { left :: f l
                                               , right :: f r }

  filterZip :: forall x y o.
    (These x y -> Maybe o) -> f x -> f y -> f o

  zipWith :: forall x y o.
    (x -> y -> o) -> f x -> f y -> f o

zip :: forall f x y. Zippable f =>
  f x -> f y -> f (Tuple x y)
zip = zipWith Tuple

