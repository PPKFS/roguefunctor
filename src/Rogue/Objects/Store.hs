module Rogue.Objects.Store
  ( -- * Stores
    Store(..)
  , emptyStore
  , insert
  , lookup
  , unsafeLookup
  , update
  , updateM
  ) where

import Rogue.Prelude
import Rogue.Objects.Entity
import qualified Data.EnumMap as EM
import qualified Data.IntMap as IM

-- | An `EM.EnumMap` specialised over `Entity`s.
newtype Store a = Store
  { unStore :: EM.EnumMap Entity a
  } deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Read, Foldable, Functor)

-- | A store with no items.
emptyStore :: Store a
emptyStore = Store EM.empty

alterEMF ::
  (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))
  -> k
  -> EM.EnumMap k a
  -> f (EM.EnumMap k a)
alterEMF upd k m = EM.intMapToEnumMap <$> IM.alterF upd (fromEnum k) (EM.enumMapToIntMap m)

alterNewtypeEMF ::
  (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))
  -> k
  -> (nt -> EM.EnumMap k a)
  -> (EM.EnumMap k a -> nt)
  -> nt
  -> f nt
alterNewtypeEMF upd k unwrap wrap' m = wrap' <$> alterEMF upd k (unwrap m)

instance At (Store a) where
  at k = lensVL $ \f -> alterNewtypeEMF f k unStore Store

instance Traversable Store where
  sequenceA (Store s) = Store <$> sequenceA s

type instance IxValue (Store a) = a
type instance Index (Store a) = Entity
instance Ixed (Store a)

insert :: Entity -> a -> Store a -> Store a
insert e a = at e ?~ a

lookup :: Entity -> Store a -> Maybe a
lookup e s = s ^? at e % _Just

unsafeLookup :: Entity -> Store a -> a
unsafeLookup e = fromMaybe (error $ "failed to lookup " <> show e) . lookup e

update :: Entity -> (a -> a) -> Store a -> Store a
update e f = at e % _Just %~ f

updateM :: Monad m => Entity -> (a -> m a) -> Store a -> m (Store a)
updateM e f s = do
  let a = lookup e s
  case a of
    Nothing -> pure s
    Just a' -> f a' >>= \fa -> return $ insert e fa s

traverseStore :: (a -> Maybe a) -> Store a -> (Store a, [a])
traverseStore = error ""