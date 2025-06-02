module Rogue.Monad where

import Rogue.Prelude
import System.Random.Stateful
import Rogue.Objects.Entity
import Optics.State.Operators ((%=))
import Optics.State (use)
import Rogue.Objects.Object

data RogueState = RogueState
  { rng :: AtomicGen StdGen
  , currentEntity :: Entity
  } deriving stock (Eq, Show, Generic)

class Monad m => MonadRogue m where
  getRogueState :: m RogueState
  setRogueState :: RogueState -> m ()
  generateEntity :: m Entity

instance Monad m => MonadRogue (StateT RogueState m) where
  getRogueState = get
  setRogueState = put
  generateEntity = do
    e <- use #currentEntity
    #currentEntity %= (+1)
    return e

newtype RogueT m a = RogueT { runRogueT :: StateT RogueState m a }
  deriving stock (Generic, Functor)
  deriving newtype (Applicative, Monad, MonadState RogueState, MonadIO, MonadTrans)

instance Monad m => MonadRogue (RogueT m) where
  getRogueState = get
  setRogueState = put
  generateEntity = do
    e <- use #currentEntity
    #currentEntity %= (+1)
    return e

instance (Monad m, MonadTrans t) => MonadRogue (t (RogueT m)) where
  getRogueState = lift getRogueState
  setRogueState = lift . setRogueState
  generateEntity = lift generateEntity

makeObject :: MonadRogue m => ObjectKind -> Text -> objData -> objSpecifics -> m (Object objData objSpecifics)
makeObject k n d s = do
  e <- generateEntity
  return (Object n e k (Timestamp 0) (Timestamp 0) d s)