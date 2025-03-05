{-# LANGUAGE PatternSynonyms #-}

module Rogue.Geometry.V2
  ( V2(..)
  , withV2
  , convertV2
  , pattern WithV2
  , dot
  , tupleToV2
  , squaredDistance
  ) where

import Data.MonoTraversable
import Relude
import GHC.Ix (Ix(..))
import Optics
import Data.Hashable
import Data.Aeson
import qualified Data.Vector as V

data V2 = V2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show, Read, Generic)

instance FromJSON V2 where
  parseJSON = withArray "V2" $ \v -> case (toList $ V.take 2 v) of
    [x, y] -> V2 <$> parseJSON x <*> parseJSON y
    _ -> error "invalid V2 fromJSON"

withV2 ::
  V2
  -> (Int -> Int -> r)
  -> r
withV2 (V2 x y) f = f x y
{-# INLINE withV2 #-}

convertV2 ::
  Coercible v V2
  => (Int -> a)
  -> (a -> a -> r)
  -> v
  -> r
convertV2 f c v = withV2 (coerce v) $ \x y -> c (f x) (f y)
{-# INLINE convertV2 #-}

pattern WithV2 ::
  Int
  -> Int
  -> V2
pattern WithV2 x y <- ((`withV2` (,)) -> (x, y))
{-# COMPLETE WithV2 #-}

tupleToV2 ::
  (Int, Int)
  -> V2
tupleToV2 = uncurry V2
{-# INLINE tupleToV2 #-}

dot ::
  V2
  -> V2
  -> Int
dot (V2 x1 y1) (V2 x2 y2) =
  x1 * x2 + y1 * y2
{-# INLINE dot #-}

squaredDistance ::
  V2
  -> V2
  -> Int
squaredDistance (V2 x1 y1) (V2 x2 y2) = (x2-x1)^(2 :: Int) + (y2-y1)^(2 :: Int)
{-# INLINE squaredDistance #-}

instance Hashable V2 where
  hash (V2 x y) = hash (x, y)
instance Num V2 where

  V2 x1 y1 + V2 x2 y2 = V2 (x1 + x2) (y1 + y2)
  {-# INLINE (+) #-}

  V2 x1 y1 - V2 x2 y2 = V2 (x1 - x2) (y1 - y2)
  {-# INLINE (-) #-}

  V2 x1 y1 * V2 x2 y2 = V2 (x1 * x2) (y1 * y2)
  {-# INLINE (*) #-}

  abs (V2 x y) = V2 (abs x) (abs y)
  {-# INLINE abs #-}

  signum (V2 x y) = V2 (signum x) (signum y)
  {-# INLINE signum #-}

  fromInteger x = V2 x' x'
    where
      x' = fromInteger x
  {-# INLINE fromInteger #-}

instance Ix V2 where
  range (V2 x1 y1, V2 x2 y2) = V2 <$> range (x1, x2) <*> range (y1, y2)
  {-# INLINE range #-}

  unsafeIndex (V2 x1 y1, V2 x2 y2) (V2 x3 y3) = unsafeIndex (y1, y2) y3 + unsafeRangeSize (y1, y2) * unsafeIndex (x1, x2) x3
  {-# INLINE unsafeIndex #-}

  inRange (V2 x1 y1, V2 x2 y2) (V2 x3 y3) = inRange (x1, x2) x3 && inRange (y1, y2) y3
  {-# INLINE inRange #-}

type instance Element V2 = Int

instance MonoFunctor V2 where
  omap f v = withV2 v $ \x y -> V2 (f x) (f y)
  {-# INLINE omap #-}

instance MonoPointed V2 where
  opoint x = V2 x x
  {-# INLINE opoint #-}

instance Field1 V2 V2 Int Int where
  _1 = lens (\(V2 x _) -> x) (\(V2 _ y) s -> V2 s y)

instance Field2 V2 V2 Int Int where
  _2 = lens (\(V2 _ y) -> y) (\(V2 x _) s -> V2 x s)

instance LabelOptic "x" A_Lens V2 V2 Int Int where
  labelOptic = _1

instance LabelOptic "y" A_Lens V2 V2 Int Int where
  labelOptic = _2