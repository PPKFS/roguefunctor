
module Rogue.Property.Has (
  -- * Has
    MayHaveProperty(..)
  , thenATraverse
  ) where

import Rogue.Prelude

eitherJoin ::
  AffineTraversal' a f
  -> AffineTraversal' b f
  -> AffineTraversal' (Either a b) f
eitherJoin t1 t2 = (_Left % t1) `thenATraverse` (_Right % t2)

thenATraverse
  :: Is t1 An_AffineTraversal
  => Is t2 An_AffineTraversal
  => Optic t1 ix s s a b
  -> Optic t2 ix s s a b
  -> AffineTraversal s s a b
thenATraverse o1 o2 = atraversal
  ( \s -> case matching o1 s of
      Left _ -> matching o2 s
      Right f -> Right f
  )
  (\s b -> s & castOptic @An_AffineTraversal o1 .~ b & castOptic @An_AffineTraversal o2 .~ b)

-- | An `AffineTraversal` is an optic that focuses on 0-1 objects; it's a `Prism` without
-- the condition that you can build it back up again..which works great for the possibility
-- that our world model instantiation of object specifics may contain many possible pathways
-- for any individual property but there's no way to do the Prism review.
class MayHaveProperty o v where
  default propertyAT :: AffineTraversal' o v
  propertyAT = atraversal Left const
  propertyAT :: AffineTraversal' o v

instance (MayHaveProperty a v, MayHaveProperty b v) => MayHaveProperty (Either a b) v where
  propertyAT = propertyAT `eitherJoin` propertyAT

instance MayHaveProperty a v => MayHaveProperty (Maybe a) v where
  propertyAT = atraversal (\case
    Nothing -> Left Nothing
    Just x -> case x ^? propertyAT of
      Nothing -> Left $ Just x
      Just y -> Right y)
    (\case
      Nothing -> const Nothing
      Just a -> \v -> Just $ a & propertyAT .~ v)

class HasProperty w o v where
  propertyL :: w -> Lens' o v

-- not sure about this one
instance MayHaveProperty o v => HasProperty w o v where
  propertyL _ = lens (fromMaybe (error "property witness was violated") . preview propertyAT) (flip (set propertyAT))
