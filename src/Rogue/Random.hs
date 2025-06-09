module Rogue.Random
  ( randomPoint
  , random
  , randomV2
  , coinFlip
  , choose
  , randomEnum
  ) where

import Rogue.Prelude
import Rogue.Monad
import System.Random.Stateful hiding (random, uniform)
import qualified System.Random as R
import GHC.List ((!!))

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

randomEnum :: forall a m. (Enum a, Bounded a, MonadRogue m) => m a
randomEnum = toEnum <$> uniform (fromEnum @a minBound) (fromEnum @a maxBound)

choose :: MonadRogue m => [a] -> m a
choose l = do
  i <- uniform 0 (length l - 1)
  return $ l !! i

coinFlip :: MonadRogue m => m Bool
coinFlip = random