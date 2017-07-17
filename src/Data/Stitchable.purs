module Data.Stitchable
  ( class Stitchable
  , stitchedWith
  , stitched
  , stitch
  , module Data.Zippable
  ) where

import Control.Applicative (class Applicative, pure)
import Data.Zippable (class Zippable, zipped)
import Data.Functor ((<$>))
import Data.Filterable (class Filterable, filtered)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.These (These(..))

-- | `Stitchable` represents data structures which support being _zipped_
-- | together with effects in some `Applicative` functor.
-- |
-- | - `stitchedWith` - TODO
-- |
-- | Laws:
-- |
-- | - TODO
-- |
-- | Superclass equivalences:
-- |
-- | - TODO
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - TODO
class Zippable f <= Stitchable f where
  stitchedWith :: forall m x y o.
    (These x y -> m o) -> f x -> f y -> m (f o)

stitched :: forall f m x y. Zippable f => Applicative m =>
  f x -> f y -> m (f (These x y))
stitched xs ys = pure (zipped xs ys)

stitchWith :: forall f m x y o. Stitchable f => Filterable f => Applicative m =>
  (x -> y -> m o) -> f x -> f y -> m (f o)
stitchWith f xs ys = filtered <$> (stitchedWith applyOnBoth xs ys) where  
  applyOnBoth (This x) = pure Nothing
  applyOnBoth (That y) = pure Nothing
  applyOnBoth (Both x y) = Just <$> f x y

stitch :: forall f m x y. Stitchable f => Filterable f => Applicative m =>
  f x -> f y -> m (f (Tuple x y))
stitch = stitchWith (\x y -> pure (Tuple x y))

