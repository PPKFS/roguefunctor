module Rogue.Tilemap where
import Rogue.Geometry.V2
import qualified Rogue.Array2D.Unboxed as AU
import qualified Data.Vector.Unboxed as VU
import qualified Rogue.Array2D.Boxed as A
import Rogue.Prelude

class WalkabilityMap map where
  positionBlocksMovement :: map -> V2 -> Bool

class TileWalkability tile where
  walkability :: tile -> Bool

instance TileWalkability tile => WalkabilityMap (A.Array2D tile) where
  positionBlocksMovement a = walkability . (a A.!@)

positionAllowsMovement :: WalkabilityMap map => map -> V2 -> Bool
positionAllowsMovement m = not . positionBlocksMovement m

class VisibilityMap map where
  positionBlocksVisibility :: map -> V2 -> Bool

class TileVisibility tile where
  visibility :: tile -> Bool

instance TileVisibility tile => VisibilityMap (A.Array2D tile) where
  positionBlocksVisibility a =  visibility . (a A.!@)

class Tilemap tilemap tilekind | tilemap -> tilekind where
  getTile :: tilemap -> V2 -> tilekind
  setTile :: tilemap -> tilekind -> V2 -> tilemap
  setTiles :: tilemap -> [(V2, tilekind)] -> tilemap
  setTiles = foldl' (\tm' (v, k) -> setTile tm' k v)
  {-# MINIMAL getTile, setTile #-}


instance VU.Unbox a => Tilemap (AU.Array2D a) a where
  getTile = (AU.!@)
  setTile a v p = a AU.//@ [(p, v)]
  setTiles = (AU.//@)

instance Tilemap (A.Array2D a) a where
  getTile = (A.!@)
  setTile a v p = a A.//@ [(p, v)]
  setTiles = (A.//@)
