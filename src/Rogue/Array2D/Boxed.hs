module Rogue.Array2D.Boxed where

import qualified Data.Vector as V
import Rogue.Prelude

newtype Array2D a = Array2D { unArray :: (V.Vector a, V2) }
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord)
  deriving stock (Functor)

dimensions :: Array2D a -> V2
dimensions = snd . unArray

indexToCoord :: Int -> Int -> V2
indexToCoord w i = V2 (i `mod` w) (i `div` w)

coordToIndex :: Int -> V2 -> Int
coordToIndex w (V2 x y) = y*w + x

(!@) :: Array2D a -> V2 -> a
(Array2D (arr, V2 w _)) !@ v = arr V.! coordToIndex w v

(!?@) :: Array2D a -> V2 -> Maybe a
(Array2D (arr, V2 w _)) !?@ v = arr V.!? coordToIndex w v

(//@) :: Array2D a -> [(V2, a)] -> Array2D a
(//@) (Array2D (arr, dims@(V2 w _))) l = Array2D (arr V.// map (first $ coordToIndex w) l, dims)

(//) :: Array2D a -> (V2, a) -> Array2D a
(//) (Array2D (arr, dims@(V2 w _))) l = Array2D (arr V.// [(first $ coordToIndex w) l], dims)

toVector :: Array2D a -> V.Vector a
toVector (Array2D (a, _)) = a

traverseArray :: Monad m => Array2D a -> (a -> m b) -> m (V.Vector b)
traverseArray (Array2D (arr, _)) = V.forM arr

traverseArray_ :: Monad m => Array2D a -> (a -> m b) -> m ()
traverseArray_ (Array2D (arr, _)) = V.forM_ arr

traverseArrayWithCoord :: Monad m => Array2D a -> (V2 -> a -> m b) -> m (V.Vector b)
traverseArrayWithCoord (Array2D (arr, V2 w _)) f = V.iforM arr $ \i v -> f (indexToCoord w i) v

traverseArrayWithCoord_ :: Monad m => Array2D a -> (V2 -> a -> m b) -> m ()
traverseArrayWithCoord_ (Array2D (arr, V2 w _)) f = V.iforM_ arr $ \i v -> f (indexToCoord w i) v

replicateArray :: a -> V2 -> Array2D a
replicateArray a d@(V2 x y) = Array2D (V.replicate (x*y) a, d)

instance Ixed (Array2D a) where
  ix k = atraversal (\v -> maybeToRight v (v !?@ k)) (\v t -> v // (k, t))

type instance IxValue (Array2D a) = a
type instance Index (Array2D a) = V2