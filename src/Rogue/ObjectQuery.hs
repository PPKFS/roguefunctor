module Rogue.ObjectQuery where

import Rogue.Prelude
import Effectful.TH
import Rogue.Property.Has
import Effectful


class HasID n where
  type Id n
  -- | Get an ID.
  getID :: n -> Id n

class HasSpecifics o where
  type Specifics o
  specificsL :: Lens' o (Specifics o)

data ObjectQuery o :: Effect where
  GenerateEntity :: ObjectQuery o m (Id o)
  GetObject :: Id o -> ObjectQuery o m o
  SetObject :: o -> ObjectQuery o m ()
  TraverseObjects :: (o -> m (Maybe o)) -> ObjectQuery o m ()

makeEffect ''ObjectQuery

addObject ::
  ObjectQuery obj :> es
  => (Id obj -> obj)
  -> Eff es ()
addObject mkO = do
  e <- generateEntity
  setObject (mkO e)

modifyObject ::
  ObjectQuery obj :> es
  => Id obj
  -> (obj -> obj)
  -> Eff es ()
modifyObject e m = do
  obj <- getObject e
  let newObj = m obj
  --ts <- getGlobalTime
  setObject newObj -- (newObj & modifiedL .~ ts)

defaultPropertySetter ::
  (HasID o, HasSpecifics o, MayHaveProperty (Specifics o) p, ObjectQuery o :> es)
  => o
  -> p
  -> Eff es ()
defaultPropertySetter e v = modifyObject (getID e) (specificsL % propertyAT .~ v)

defaultPropertyGetter ::
  (HasSpecifics o, MayHaveProperty (Specifics o) p)
  => o
  -> Maybe p
defaultPropertyGetter = preview (specificsL % propertyAT)

modifyProperty ::
  (o -> Maybe p)
  -> (o -> p -> Eff es ())
  -> o
  -> (p -> p)
  -> Eff es ()
modifyProperty g s o f = do
  let e = g o
  when (isNothing e) (do
    --logVerbose "Trying to modify a property of an object which does not exist"
    pass)
  whenJust e (s o . f)