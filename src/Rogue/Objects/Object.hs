{-|
Module      : Yaifl.Core.Kinds.Object
Copyright   : (c) Avery 2023-2024
License     : MIT
Maintainer  : ppkfs@outlook.com

A game object (a thing or a room).
-}

module Rogue.Objects.Object (
  -- * Objects
  -- ** Objects
  Object(..)
  , Timestamp(..)
  , ObjectKind(..)
  , objectEquals
  ) where

import Rogue.Prelude
import Rogue.Objects.Entity

-- | See also `Yaifl.Core.Metadata.typeDAG`. An object type is just a string that has some relations to other types.
-- there is no data or polymorphism connected to a type, so it's very possible to call something a supporter without
-- having some supporter properties.
newtype ObjectKind = ObjectKind
  { unObjectKind :: Text
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, IsString, Monoid, Semigroup)

-- | A `Timestamp` is used to date events that modify, add, or remove objects.
-- Currently these aren't...used for anything.
newtype Timestamp = Timestamp
  { unTimestamp :: Int
  } deriving stock (Show, Read, Generic)
    deriving newtype (Eq, Num, Enum, Ord, Real, Integral)

-- | A game object.
data Object objData objSpecifics = Object
  { name :: Text
  , objectId :: Entity
  , objectKind :: ObjectKind
  , creationTime :: Timestamp
  , modifiedTime :: Timestamp
  , objectData :: objData
  , specifics :: objSpecifics -- ^ A @vanilla@ object has no specific additional information; this is a @Pointed@ constraint.
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''Object

-- | By generalising `Eq`, we can compare two objects of different kinds. Trivially this is always `False`,
-- but it does allow comparing a `Thing` and an `AnyObject`.
objectEquals ::
  HasID a
  => HasID b
  => a
  -> b
  -> Bool
objectEquals = (. getID) . (==) . getID

instance Eq (Object d s) where
  (==) = objectEquals

-- | Maybe I'll need this instance for something or other?
instance Ord (Object d s) where
  compare = (. creationTime) . compare . creationTime

instance HasID (Object d s) where
  getID = objectId

instance Functor (Object d) where
  fmap f = #specifics %~ f

instance Bifunctor Object where
  bimap f g o = o & #objectData %~ f & #specifics %~ g

instance Bifoldable Object where
  bifoldMap f g o = f (o ^. #objectData) <> g (o ^. #specifics)

instance Bitraversable Object where
  bitraverse f g o =
    let d' = f (objectData o)
        s' = g (specifics o)
    in (\d s -> o & #objectData .~ d & #specifics .~ s) <$> d' <*> s'

