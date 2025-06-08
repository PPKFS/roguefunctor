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

instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadRogue m) => MonadRogue (t m) where
  getRogueState = lift getRogueState
  setRogueState = lift . setRogueState
  generateEntity = lift generateEntity

makeObject :: MonadRogue m => ObjectKind -> Text -> objData -> objSpecifics -> m (Object objData objSpecifics)
makeObject k n d s = do
  e <- generateEntity
  return (Object n e k (Timestamp 0) (Timestamp 0) d s)

class Monad m => MonadStore storeType m where
  getObject :: HasID a => a -> m storeType
  setObject :: storeType -> m ()

traverseObjects :: (MonadStore a m, Traversable f) => m (f a) -> (a -> m (Maybe a)) -> m [a]
traverseObjects getS f = do
  m <- getS
  toList <$> mapM (\aT -> do
      r <- f aT
      whenJust r setObject
      return (fromMaybe aT r)) m

traverseObjects_ :: (MonadStore a m, Traversable f) => m (f a) -> (a -> m (Maybe a)) -> m ()
traverseObjects_ g f = void (traverseObjects g f)

modifyObject :: (MonadStore a m, HasID a) => a -> (a -> a) -> m ()
modifyObject e f = do
  o <- getObject e
  setObject (f o)