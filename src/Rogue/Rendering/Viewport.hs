{-# LANGUAGE RecordWildCards #-}
module Rogue.Rendering.Viewport where
import Rogue.Geometry.Rectangle
import Rogue.Prelude hiding (Reader, runReader, ask)
import Effectful.Reader.Static
import Rogue.Colour
import Data.Ix (Ix(inRange))

type Layer = Word8

bgLayer :: Layer
bgLayer = 0

-- | a viewport is some rectangular area over some amount of layers
-- two viewports cannot clash in both layer and area, so each one can be cleared and rendered
-- independently. This does mean a viewport does not hold any specific logic itself

data Viewport layer = Viewport
  { viewport :: Rectangle
  , backgroundLayer :: Maybe Colour
  , border :: Maybe (BorderTileSet, Colour)
  }

class AsLayer layer where
  toLayer :: layer -> Word8

terminalLayer' :: IOE :> es => Word8 -> Eff es ()
terminalLayer' = terminalLayer . fromIntegral

withViewport ::
  Viewport l
  -> Eff (Reader (Viewport l) : es) a
  -> Eff es a
withViewport = runReader

renderViewport ::
  IOE :> es
  => AsLayer l
  => Enum l
  => Bounded l
  => Viewport l
  -> Eff (Reader (Viewport l) : es) a
  -> Eff es a
renderViewport v f = withViewport v $ do
  clearViewport v
  whenJust (border v) $ const borderViewport
  f

clearViewport ::
  forall l es.
  IOE :> es
  => AsLayer l
  => Enum l
  => Bounded l
  => Viewport l
  -> Eff es ()
clearViewport v = forM_ (universe @l) $ \dl -> do
  terminalLayer' (toLayer dl)
  -- if this is the global background layer
  when (toLayer dl == bgLayer) $
    whenJust (backgroundLayer v) terminalBkColour
  let V2 tx ty = topLeft (viewport v)
      V2 bx by = rectangleDimensions (viewport v)
  terminalClearArea tx ty bx by

withViewportTransform ::
  Reader (Viewport l) :> es
  => V2
  -> (Int -> Int -> Eff es a)
  -> Eff es a
withViewportTransform p f = do
  r <- ask
  withV2 (topLeft (viewport r) + p) f

whenInViewport ::
  MonadIO m
  => Viewport l
  -> V2
  -> m ()
  -> m ()
whenInViewport v p = when (inRange (V2 0 0, V2 1 1 + rectangleDimensions (viewport v)) p)

viewportDrawTile ::
  IOE :> es
  => AsLayer l
  => Reader (Viewport l) :> es
  => V2
  -> Maybe l
  -> Colour
  -> Char
  -> Eff es ()
viewportDrawTile p mbL fg glyph = do
  whenJust mbL $ do
    terminalLayer' . toLayer
  terminalColour fg
  void $ withViewportTransform p (\x y -> do
    -- print ("original " <> show p <> "transformed " <> show (x, y) <> "for" <> show glyph)
    terminalPrintText x y (one glyph))

viewportPrint ::
  IOE :> es
  => AsLayer l
  => Reader (Viewport l) :> es
  => V2
  -> Maybe l
  -> Colour
  -> Text
  -> Eff es ()
viewportPrint p mbL fg str = do
  whenJust mbL $ do
    terminalLayer' . toLayer
  terminalColour fg
  void $ withViewportTransform p (\x y -> do
    -- print ("original " <> show p <> "transformed " <> show (x, y) <> "for" <> show glyph)
    terminalPrintText x y str)

data BorderTileSet = BTS
  { tl :: Char
  , tr :: Char
  , bl :: Char
  , br :: Char
  , l :: Char
  , r :: Char
  , t :: Char
  , b :: Char
  }

unicodeBorders :: BorderTileSet
unicodeBorders = BTS
  { tl = '╔'
  , tr = '╗'
  , bl = '╚'
  , br = '╝'
  , l = '║'
  , r = '║'
  , t = '═'
  , b = '═'
  }

borderViewport ::
  IOE :> es
  => AsLayer l
  => Reader (Viewport l) :> es
  => Eff es ()
borderViewport = do
  (Viewport oldRect _ mbBs) <- ask
  whenJust mbBs $ \(BTS{..}, c) -> do
    let f v = viewportDrawTile v Nothing c
    terminalColour c
    let rect = moveToOrigin oldRect
    f (V2 0 0) tl
    f (bottomRight rect - V2 1 1) br
    f (topRight rect - V2 1 0) tr
    f (bottomLeft rect - V2 0 1) bl
    forM_ [view _1 (topLeft rect) + 1 .. view _1 (topRight rect) - 2 ] $ \x -> do
      f (V2 x (rect ^. #topLeft % _2)) t
      f (V2 x (rect ^. #bottomRight % _2 - 1)) b
    forM_ [view _2 (topLeft rect) + 1 .. view _2 (bottomLeft rect) - 2 ] $ \y -> do
      f (V2 (rect ^. #topLeft % _1) y) l
      f (V2 (rect ^. #bottomRight % _1 - 1) y) r

data SomeViewport es where
  SomeViewport :: (IOE :> es, AsLayer l, Eq l, Enum l, Bounded l) => Viewport l -> Eff (Reader (Viewport l) : es) () -> SomeViewport es