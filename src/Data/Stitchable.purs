module Data.Stitchable
  ( class Stitchable
  , wiltStitch
  , witherStitch
  , stitchWith
  ) where

import Control.Applicative (class Applicative, pure)
import Data.Zippable (class Zippable)
import Data.Witherable (class Witherable)
import Data.Either (Either())
import Data.Maybe (Maybe())
import Data.Tuple (Tuple(..))
import Data.These (These())

-- | `Stitchable` represents data structures which support being _zipped_
-- | together with effects in some `Applicative` functor.
-- |
-- | - `wiltStitch` - TODO
-- | - `witherStitch` - TODO
-- | - `stitchWith` - TODO
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
class (Zippable t, Witherable t) <= Stitchable t where
  wiltStitch :: forall m x y l r. Applicative m =>
    (These x y -> m (Either l r)) -> t x -> t y -> m { left :: t l
                                                     , right :: t r }

  witherStitch :: forall m x y o. Applicative m =>
    (These x y -> m (Maybe o)) -> t x -> t y -> m (t o)

  stitchWith :: forall m x y o. Applicative m =>
    (x -> y -> m o) -> t x -> t y -> m (t o)

stitch :: forall m t x y. (Stitchable t, Applicative m) =>
  t x -> t y -> m (t (Tuple x y))
stitch = stitchWith (\x y -> pure (Tuple x y))

