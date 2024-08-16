module Rogue.Array2D.Sparse
  ( SparseArray2D(..)
  , emptySparseArray
  ) where

import Rogue.Prelude
import qualified Data.IntMap as IM

data SparseArray2D a = SparseArray2D
  { sparseMap :: IM.IntMap a
  , dimensions :: V2
  } deriving stock (Generic, Show, Eq, Ord, Functor)

emptySparseArray ::
  V2
  -> SparseArray2D a
emptySparseArray dimensions = SparseArray2D { sparseMap = IM.empty, dimensions }

indexToCoord :: Int -> Int -> V2
indexToCoord w i = V2 (i `mod` w) (i `div` w)

coordToIndex :: Int -> V2 -> Int
coordToIndex w (V2 x y) = y*w + x

instance At (SparseArray2D a) where
  at k = lens
    (\s -> s ^. #sparseMap % at (coordToIndex (s ^. #dimensions % _1) k))
    (\s e -> s & #sparseMap % at (coordToIndex (s ^. #dimensions % _1) k) .~ e)

type instance IxValue (SparseArray2D a) = a
type instance Index (SparseArray2D a) = V2
instance Ixed (SparseArray2D a)