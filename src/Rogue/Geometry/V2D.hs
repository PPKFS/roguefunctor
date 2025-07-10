{-|
Module      : Rogue.Geometry.V2D
Description : 2D vector types with double precision floating coordinates.
License     : MIT
Stability   : experimental
Portability : POSIX

A 2D vector type (i.e. isomorphic to (a, a)) that uses `Double` instead of `Int` as the underlying component.
-}


{-# LANGUAGE PatternSynonyms #-}

module Rogue.Geometry.V2D
  ( V2D(..)
  , withV2D
  , pattern WithV2D
  , dot
  , tupleToV2D
  , squaredDistance
  , modifyX
  , modifyY
  , v2DAsArea
  , fromV2
  ) where

import Data.MonoTraversable
import Relude
import Optics
import Data.Hashable
import Data.Aeson
import qualified Data.Vector as V
import Rogue.Geometry.V2 (V2 (..))

-- | A 2D vector type for representing coordinates, points, etc. This one uses doubles (i.e. floating point), if you need more granularity
-- than just integer coordinates. See also `Rogue.Geometry.V2`.
data V2D = V2D {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving stock (Eq, Ord, Show, Read, Generic)

instance FromJSON V2D where
  parseJSON = withArray "V2D" $ \v -> case toList $ V.take 2 v of
    [x, y] -> V2D <$> parseJSON x <*> parseJSON y
    _ -> error "invalid V2D fromJSON"

withV2D ::
  V2D
  -> (Double -> Double -> r)
  -> r
withV2D (V2D x y) f = f x y
{-# INLINE withV2D #-}

pattern WithV2D ::
  Double
  -> Double
  -> V2D
pattern WithV2D x y <- ((`withV2D` (,)) -> (x, y))
{-# COMPLETE WithV2D #-}

tupleToV2D ::
  (Double, Double)
  -> V2D
tupleToV2D = uncurry V2D
{-# INLINE tupleToV2D #-}

dot ::
  V2D
  -> V2D
  -> Double
dot (V2D x1 y1) (V2D x2 y2) =
  x1 * x2 + y1 * y2
{-# INLINE dot #-}

squaredDistance ::
  V2D
  -> V2D
  -> Double
squaredDistance (V2D x1 y1) (V2D x2 y2) = (x2-x1)^(2 :: Int) + (y2-y1)^(2 :: Int)
{-# INLINE squaredDistance #-}

instance Hashable V2D where
  hash (V2D x y) = hash (x, y)
instance Num V2D where

  V2D x1 y1 + V2D x2 y2 = V2D (x1 + x2) (y1 + y2)
  {-# INLINE (+) #-}

  V2D x1 y1 - V2D x2 y2 = V2D (x1 - x2) (y1 - y2)
  {-# INLINE (-) #-}

  V2D x1 y1 * V2D x2 y2 = V2D (x1 * x2) (y1 * y2)
  {-# INLINE (*) #-}

  abs (V2D x y) = V2D (abs x) (abs y)
  {-# INLINE abs #-}

  signum (V2D x y) = V2D (signum x) (signum y)
  {-# INLINE signum #-}

  fromInteger x = V2D x' x'
    where
      x' = fromInteger x
  {-# INLINE fromInteger #-}

type instance Element V2D = Double

instance MonoFunctor V2D where
  omap f v = withV2D v $ \x y -> V2D (f x) (f y)
  {-# INLINE omap #-}

instance MonoPointed V2D where
  opoint x = V2D x x
  {-# INLINE opoint #-}

instance Field1 V2D V2D Double Double where
  _1 = lens (\(V2D x _) -> x) (\(V2D _ y) s -> V2D s y)

instance Field2 V2D V2D Double Double where
  _2 = lens (\(V2D _ y) -> y) (\(V2D x _) s -> V2D x s)

instance LabelOptic "x" A_Lens V2D V2D Double Double where
  labelOptic = _1

instance LabelOptic "y" A_Lens V2D V2D Double Double where
  labelOptic = _2

modifyX :: (Double -> Double) -> V2D -> V2D
modifyX f (V2D x y) = V2D (f x) y

modifyY :: (Double -> Double) -> V2D -> V2D
modifyY f (V2D x y) = V2D x (f y)

v2DAsArea :: V2D -> Double
v2DAsArea (V2D x y) = x * y

fromV2 :: V2 -> V2D
fromV2 (V2 x y) = V2D (fromIntegral x) (fromIntegral y)