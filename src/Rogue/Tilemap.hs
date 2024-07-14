module Rogue.Tilemap where
import Rogue.Geometry.V2
import qualified Rogue.Array2D.Unboxed as AU
import qualified Data.Vector.Unboxed as VU
import qualified Rogue.Array2D.Boxed as A
import Rogue.Prelude

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

class KeyedTilemap tilemap key tilekind | tilemap -> key, tilemap -> tilekind where
  getTileKind :: tilemap -> V2 -> tilekind
  setTileKind :: tilemap -> key -> V2 -> tilemap
  setTileKinds :: tilemap -> [(V2, key)] -> tilemap
  setTileKinds = foldl' (\tm' (v, k) -> setTileKind tm' k v)
  {-# MINIMAL getTileKind, setTileKind #-}

class WalkabilityMap map where
  dimensions :: map -> V2
  positionBlocksMovement :: map -> V2 -> Bool

class TileWalkability tile where
  walkability :: Getter tile Bool

instance TileWalkability tile => WalkabilityMap (A.Array2D tile) where
  dimensions (A.Array2D (_, d)) = d
  positionBlocksMovement a = view walkability . getTile a
{-}

data Tiles tileIndex tilekind = Tiles
  { tileKinds :: IntMap tilekind
  , tileMap :: AU.Array2D tileIndex
  } deriving stock (Generic, Show)

instance VU.Unbox tileIndex => Tilemap (Tiles tileIndex tilekind) tileIndex where
  getTile = (AU.!@) . tileMap
  setTile t v p = t & #tileMap %~ (\tm -> tm AU.//@ [(p, v)])
  setTiles t ls = t & #tileMap %~ (AU.//@ ls)

instance (VU.Unbox tileIndex, Enum tileIndex) => KeyedTilemap (Tiles tileIndex tilekind) tileIndex tilekind where
  getTileKind tiles p = getTile tiles p & (\tk -> tileKinds tiles IM.! fromEnum tk)
  setTileKind = setTile
  setTileKinds = setTiles
-}