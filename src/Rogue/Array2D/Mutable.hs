module Rogue.Array2D.Mutable where
import qualified Data.Vector.Mutable as V
import Rogue.Prelude

newtype Array2D a = Array2D { unArray :: (V.IOVector a, V2) }
  deriving stock (Generic)

dimensions :: Array2D a -> V2
dimensions = snd . unArray

indexToCoord :: Int -> Int -> V2
indexToCoord w i = V2 (i `mod` w) (i `div` w)

coordToIndex :: Int -> V2 -> Int
coordToIndex w (V2 x y) = y*w + x

(!@) :: MonadIO m => Array2D a -> V2 -> m a
(Array2D (arr, V2 w _)) !@ v = liftIO $ arr `V.read` coordToIndex w v

(!?@) :: Array2D a -> V2 -> m (Maybe a)
(Array2D (arr, V2 w _)) !?@ v = liftIO $ arr `V.readMaybe` coordToIndex w v

(//@) :: Array2D a -> [(V2, a)] -> Array2D a
(//@) (Array2D (arr, dims@(V2 w _))) l = Array2D (arr V.// map (first $ coordToIndex w) l, dims)

(//) :: Array2D a -> (V2, a) -> Array2D a
(//) (Array2D (arr, dims@(V2 w _))) l = Array2D (arr V.// [(first $ coordToIndex w) l], dims)

toVector :: Array2D a -> V.IOVector a
toVector (Array2D (a, _)) = a

traverseArray :: Monad m => Array2D a -> (a -> m b) -> m (V.IOVector b)
traverseArray (Array2D (arr, _)) = V.forM arr

traverseArray_ :: Monad m => Array2D a -> (a -> m b) -> m ()
traverseArray_ (Array2D (arr, _)) = V.forM_ arr

traverseArrayWithCoord :: Monad m => Array2D a -> (V2 -> a -> m b) -> m (V.IOVector b)
traverseArrayWithCoord (Array2D (arr, V2 w _)) f = V.iforM arr $ \i v -> f (indexToCoord w i) v

traverseArrayWithCoord_ :: Monad m => Array2D a -> (V2 -> a -> m b) -> m ()
traverseArrayWithCoord_ (Array2D (arr, V2 w _)) f = V.iforM_ arr $ \i v -> f (indexToCoord w i) v

type instance IxValue (Array2D a) = a
type instance Index (Array2D a) = V2