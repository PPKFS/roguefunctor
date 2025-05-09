module Rogue.Random
  ( randomPoint
  , random
  , randomV2
  , coinFlip
  ) where

import Rogue.Prelude
import Rogue.Monad
import System.Random.Stateful hiding (random, uniform)
import qualified System.Random as R

uniform :: (UniformRange a, MonadRogue m) => a -> a -> m a
uniform a b = do
  rs <- getRogueState
  let (x, y) = uniformR (a, b) (rng rs)
  setRogueState (rs & #rng .~ y)
  return x

randomPoint :: MonadRogue m => V2 -> V2 -> m V2
randomPoint a b = uniform a (b - V2 1 1)

randomV2 :: MonadRogue m => V2 -> V2 -> m V2
randomV2 = uniform

random :: forall a m. (MonadRogue m, Random a) => m a
random = do
  rs <- getRogueState
  let (x, y) = R.random (rng rs)
  setRogueState (rs & #rng .~ y)
  return x

coinFlip :: MonadRogue m => m Bool
coinFlip = random