module Rogue.Array2D.Unboxed where

import Rogue.Geometry.V2 ( V2(..) )
import qualified Data.Vector.Unboxed as V
import Rogue.Prelude

newtype Array2D a = Array2D (V.Vector a, V2)
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord)

indexToCoord :: Int -> Int -> V2
indexToCoord w i = V2 (i `mod` w) (i `div` w)

coordToIndex :: Int -> V2 -> Int
coordToIndex w (V2 x y) = y*w + x

(!@) :: V.Unbox a => Array2D a -> V2 -> a
(Array2D (arr, V2 w _)) !@ v = arr V.! coordToIndex w v

(!?@) :: V.Unbox a => Array2D a -> V2 -> Maybe a
(Array2D (arr, V2 w _)) !?@ v = arr V.!? coordToIndex w v

(//@) :: V.Unbox a => Array2D a -> [(V2, a)] -> Array2D a
(//@) (Array2D (arr, dims@(V2 w _))) l = Array2D (arr V.// map (first $ coordToIndex w) l, dims)

toVector :: Array2D a -> V.Vector a
toVector (Array2D (a, _)) = a

traverseArray :: V.Unbox a => V.Unbox b => Monad m => Array2D a -> (a -> m b) -> m (V.Vector b)
traverseArray (Array2D (arr, _)) = V.forM arr

traverseArray_ :: V.Unbox a => Monad m => Array2D a -> (a -> m b) -> m ()
traverseArray_ (Array2D (arr, _)) = V.forM_ arr

traverseArrayWithCoord :: V.Unbox a => V.Unbox b => Monad m => Array2D a -> (V2 -> a -> m b) -> m (V.Vector b)
traverseArrayWithCoord (Array2D (arr, V2 w _)) f = V.iforM arr $ \i v -> f (indexToCoord w i) v

traverseArrayWithCoord_ :: V.Unbox a => Monad m => Array2D a -> (V2 -> a -> m b) -> m ()
traverseArrayWithCoord_ (Array2D (arr, V2 w _)) f = V.iforM_ arr $ \i v -> f (indexToCoord w i) v