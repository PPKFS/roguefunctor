module Rogue.Monad where

import Rogue.Prelude
import System.Random.Stateful

data RogueState = RogueState
  { rng :: AtomicGen StdGen
  } deriving stock (Eq, Show, Generic)

class Monad m => MonadRogue m where
  getRogueState :: m RogueState
  setRogueState :: RogueState -> m ()

instance Monad m => MonadRogue (StateT RogueState m) where
  getRogueState = get
  setRogueState = put