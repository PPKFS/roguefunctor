module Rogue.Rendering.Viewport where
import Rogue.Geometry.Rectangle
import Rogue.Prelude hiding (Reader, runReader)
import Effectful.Reader.Static
import Rogue.Colour
import Rogue.Geometry.V2

type Layer = Word8

bgLayer :: Layer
bgLayer = 0

data Viewport layer = Viewport
  { viewport :: Rectangle
  , dirtyLayers :: [layer]
  -- if this has a background layer. if layer is bgLayer then it's done via terminalBkColour otherwise it's hacked
  , backgroundLayer :: Maybe (layer, Colour)
  }

terminalLayer' :: IOE :> es => Word8 -> Eff es ()
terminalLayer' = terminalLayer . fromIntegral

withViewport ::
  Viewport
  -> Eff (Reader Viewport : es) a
  -> Eff es a
withViewport v m = runReader v (m)

clearViewport ::
  IOE :> es
  => Viewport
  -> Eff es ()
clearViewport v = forM_ (dirtyLayers v) $ \dl -> do
  terminalLayer' dl
  when (dl == bgLayer) $
    case backgroundLayer v of
      Nothing -> error "viewport had a background layer but no colour"
      Just c -> terminalBkColour c
  withV2 (rectangleDimensions (viewport v)) $ withV2 (topLeft $ viewport v) terminalClearArea

viewportDrawTile ::
  IOE :> es
  => Reader Viewport :> es
  => V2
  -> Maybe Colour
  -> Maybe Colour
  -> Maybe Char
  -> Eff es ()
viewportDrawTile p mbBg mbFg mbGlyph = do
  whenJust mbBg $ terminalBkColour
  whenJust mbFg $ terminalBkColour
  --withV2 p terminalPrintText
