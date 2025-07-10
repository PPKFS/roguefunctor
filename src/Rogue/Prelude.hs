module Rogue.Prelude
  ( module Relude
  , module Data.Text.Display
  , module BearLibTerminal
  --, module Effectful
  , module Optics
  , module Relude.Extra.Bifunctor
  , module Relude.Extra.Tuple
  , module Rogue.Geometry.V2
  , takeWhileInclusive
  ) where

import Relude
import Optics hiding (uncons, zoom, gviews, zoomMaybe, use, gview, preuse, modifying', modifying, assign', assign)
import BearLibTerminal hiding (Dimensions(..))
import Data.Text.Display
--import Effectful
import Relude.Extra.Bifunctor
import Relude.Extra.Tuple
import Rogue.Geometry.V2

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []