module Rogue.FieldOfView.Visibility where
import Rogue.Geometry.V2
import Rogue.Prelude
import Rogue.Tilemap
import qualified Rogue.Array2D.Boxed as A
import qualified Data.Vector as V

class VisibilityMap map where
  dimensions :: map -> V2
  positionBlocksVisibility :: map -> V2 -> Bool

class TileVisibility tile where
  visibility :: Getter tile Bool

instance TileVisibility tile => VisibilityMap (A.Array2D tile) where
  dimensions (A.Array2D (_, d)) = d
  positionBlocksVisibility a = view visibility . getTile a

data Viewshed = Viewshed
  { visibleTiles :: V.Vector V2
  , range :: Int
  }

data OctantPosition = NNE | NE | SE | SSE | SSW | SW | NW | NNW

type ShadowLine = [V2]

-- this assumes y goes downwards
transformOctant :: Int -> Int -> OctantPosition -> V2
transformOctant row col = \case
  NNE -> V2 col row
  NE -> V2 row (-col)
  SE -> V2 row col
  SSE -> V2 col row
  SSW -> V2 (-col) row
  SW -> V2(-row) col
  NW -> V2 (-row) (-col)
  NNW -> V2 (-col) (-row)