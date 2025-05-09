module Rogue.Colour where

import Rogue.Prelude
import Data.Bits
import Data.Ix
import Data.Ord (clamp)
import Numeric (showHex)

newtype Colour = Colour { toWord32 :: Word32 }
  deriving stock (Generic)
  deriving newtype (Show, Read, Eq, Ord, Bits, FiniteBits, Num, Enum, Bounded, Ix, Real, Integral)

type Color = Colour

toHex :: Colour -> Text
toHex = fromString . flip showHex "" . toWord32

fromARGB :: Word8 -> Word8 -> Word8 -> Word8 -> Colour
fromARGB a r g b = (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

fromRGB :: Word8 -> Word8 -> Word8 -> Colour
fromRGB = fromARGB 0xFF

fromARGBFloat :: RealFrac a => RealFrac r => RealFrac g => RealFrac b => a -> r -> g -> b -> Colour
fromARGBFloat a r g b = fromARGB (clampScale a) (clampScale r) (clampScale g) (clampScale b)
  where
    clampScale :: RealFrac x => x -> Word8
    clampScale = round . (/255) . clamp (0, 1)

terminalColour :: MonadIO m => Colour -> m ()
terminalColour = terminalColorUInt . fromIntegral . toWord32

terminalColor :: MonadIO m => Color -> m ()
terminalColor = terminalColour

terminalBkColour :: MonadIO m => Colour -> m ()
terminalBkColour = terminalBkColorUInt . fromIntegral . toWord32

terminalBkColor :: MonadIO m => Color -> m ()
terminalBkColor = terminalColour

toGreyscale :: Color -> Color
toGreyscale (Colour c) =
  let b :: Float = fromIntegral (c .&. 0xFF)
      g :: Float = fromIntegral (c `shiftR` 8 .&. 0xFF)
      r :: Float = fromIntegral (c `shiftR` 16 .&. 0xFF)
      a = (c `shiftR` 24 .&. 0xFF)
      grey' = round (r*0.2126) + round (g*0.7152) + round (b * 0.0722)
  in
    fromARGB (fromIntegral a) grey' grey' grey'

desaturate :: Color -> Color
desaturate (Colour c) =
  let b = fromIntegral (c .&. 0xFF)
      g = fromIntegral (c `shiftR` 8 .&. 0xFF)
      r = fromIntegral (c `shiftR` 16 .&. 0xFF)
      a = (c `shiftR` 24 .&. 0xFF)
  in
    fromARGB (fromIntegral a) (r `div` 2) (g `div` 2) (b `div` 2)

black :: Colour
black = Colour 0xFF000000

white :: Colour
white = Colour 0xFFFFFFFF

indianRed :: Colour
indianRed = Colour 0xFFCD5C5C
lightCoral :: Colour
lightCoral = Colour 0xFFF08080
salmon :: Colour
salmon = Colour 0xFFFA8072
darkSalmon :: Colour
darkSalmon = Colour 0xFFE9967A
lightSalmon :: Colour
lightSalmon = Colour 0xFFFFA07A
crimson :: Colour
crimson = Colour 0xFFDC143C
red :: Colour
red = Colour 0xFFFF0000
fireBrick :: Colour
fireBrick = Colour 0xFFB22222
darkRed :: Colour
darkRed = Colour 0xFF8B0000
pink :: Colour
pink = Colour 0xFFFFC0CB
lightPink :: Colour
lightPink = Colour 0xFFFFB6C1
hotPink :: Colour
hotPink = Colour 0xFFFF69B4
deepPink :: Colour
deepPink = Colour 0xFFFF1493
mediumVioletRed :: Colour
mediumVioletRed = Colour 0xFFC71585
paleVioletRed :: Colour
paleVioletRed = Colour 0xFFDB7093
gold :: Colour
gold = Colour 0xFFFFD700
yellow :: Colour
yellow = Colour 0xFFFFFF00
lightYellow :: Colour
lightYellow = Colour 0xFFFFFFE0
lemonChiffon :: Colour
lemonChiffon = Colour 0xFFFFFACD
lightGoldenrodYellow :: Colour
lightGoldenrodYellow = Colour 0xFFFAFAD2
papayaWhip :: Colour
papayaWhip = Colour 0xFFFFEFD5
moccasin :: Colour
moccasin = Colour 0xFFFFE4B5
peachPuff :: Colour
peachPuff = Colour 0xFFFFDAB9
paleGoldenrod :: Colour
paleGoldenrod = Colour 0xFFEEE8AA
khaki :: Colour
khaki = Colour 0xFFF0E68C
darkKhaki :: Colour
darkKhaki = Colour 0xFFBDB76B
lavender :: Colour
lavender = Colour 0xFFE6E6FA
thistle :: Colour
thistle = Colour 0xFFD8BFD8
plum :: Colour
plum = Colour 0xFFDDA0DD
violet :: Colour
violet = Colour 0xFFEE82EE
orchid :: Colour
orchid = Colour 0xFFDA70D6
fuchsia :: Colour
fuchsia = Colour 0xFFFF00FF
magenta :: Colour
magenta = Colour 0xFFFF00FF
mediumOrchid :: Colour
mediumOrchid = Colour 0xFFBA55D3
mediumPurple :: Colour
mediumPurple = Colour 0xFF9370DB
rebeccaPurple :: Colour
rebeccaPurple = Colour 0xFF663399
blueViolet :: Colour
blueViolet = Colour 0xFF8A2BE2
darkViolet :: Colour
darkViolet = Colour 0xFF9400D3
darkOrchid :: Colour
darkOrchid = Colour 0xFF9932CC
darkMagenta :: Colour
darkMagenta = Colour 0xFF8B008B
purple :: Colour
purple = Colour 0xFF800080
indigo :: Colour
indigo = Colour 0xFF4B0082
slateBlue :: Colour
slateBlue = Colour 0xFF6A5ACD
darkSlateBlue :: Colour
darkSlateBlue = Colour 0xFF483D8B
mediumSlateBlue :: Colour
mediumSlateBlue = Colour 0xFF7B68EE
greenYellow :: Colour
greenYellow = Colour 0xFFADFF2F
chartreuse :: Colour
chartreuse = Colour 0xFF7FFF00
lawnGreen :: Colour
lawnGreen = Colour 0xFF7CFC00
lime :: Colour
lime = Colour 0xFF00FF00
limeGreen :: Colour
limeGreen = Colour 0xFF32CD32
paleGreen :: Colour
paleGreen = Colour 0xFF98FB98
lightGreen :: Colour
lightGreen = Colour 0xFF90EE90
mediumSpringGreen :: Colour
mediumSpringGreen = Colour 0xFF00FA9A
springGreen :: Colour
springGreen = Colour 0xFF00FF7F
mediumSeaGreen :: Colour
mediumSeaGreen = Colour 0xFF3CB371
seaGreen :: Colour
seaGreen = Colour 0xFF2E8B57
forestGreen :: Colour
forestGreen = Colour 0xFF228B22
green :: Colour
green = Colour 0xFF008000
darkGreen :: Colour
darkGreen = Colour 0xFF006400
yellowGreen :: Colour
yellowGreen = Colour 0xFF9ACD32
oliveDrab :: Colour
oliveDrab = Colour 0xFF6B8E23
olive :: Colour
olive = Colour 0xFF808000
darkOliveGreen :: Colour
darkOliveGreen = Colour 0xFF556B2F
mediumAquamarine :: Colour
mediumAquamarine = Colour 0xFF66CDAA
darkSeaGreen :: Colour
darkSeaGreen = Colour 0xFF8FBC8B
lightSeaGreen :: Colour
lightSeaGreen = Colour 0xFF20B2AA
darkCyan :: Colour
darkCyan = Colour 0xFF008B8B
teal :: Colour
teal = Colour 0xFF008080
aqua :: Colour
aqua = Colour 0xFF00FFFF
cyan :: Colour
cyan = Colour 0xFF00FFFF
lightCyan :: Colour
lightCyan = Colour 0xFFE0FFFF
paleTurquoise :: Colour
paleTurquoise = Colour 0xFFAFEEEE
aquamarine :: Colour
aquamarine = Colour 0xFF7FFFD4
turquoise :: Colour
turquoise = Colour 0xFF40E0D0
mediumTurquoise :: Colour
mediumTurquoise = Colour 0xFF48D1CC
darkTurquoise :: Colour
darkTurquoise = Colour 0xFF00CED1
cadetBlue :: Colour
cadetBlue = Colour 0xFF5F9EA0
steelBlue :: Colour
steelBlue = Colour 0xFF4682B4
lightSteelBlue :: Colour
lightSteelBlue = Colour 0xFFB0C4DE
powderBlue :: Colour
powderBlue = Colour 0xFFB0E0E6
lightBlue :: Colour
lightBlue = Colour 0xFFADD8E6
skyBlue :: Colour
skyBlue = Colour 0xFF87CEEB
lightSkyBlue :: Colour
lightSkyBlue = Colour 0xFF87CEFA
deepSkyBlue :: Colour
deepSkyBlue = Colour 0xFF00BFFF
dodgerBlue :: Colour
dodgerBlue = Colour 0xFF1E90FF
cornflowerBlue :: Colour
cornflowerBlue = Colour 0xFF6495ED
royalBlue :: Colour
royalBlue = Colour 0xFF4169E1
blue :: Colour
blue = Colour 0xFF0000FF
mediumBlue :: Colour
mediumBlue = Colour 0xFF0000CD
darkBlue :: Colour
darkBlue = Colour 0xFF00008B
navy :: Colour
navy = Colour 0xFF000080
midnightBlue :: Colour
midnightBlue = Colour 0xFF191970
cornsilk :: Colour
cornsilk = Colour 0xFFFFF8DC
blanchedAlmond :: Colour
blanchedAlmond = Colour 0xFFFFEBCD
bisque :: Colour
bisque = Colour 0xFFFFE4C4
navajoWhite :: Colour
navajoWhite = Colour 0xFFFFDEAD
wheat :: Colour
wheat = Colour 0xFFF5DEB3
burlyWood :: Colour
burlyWood = Colour 0xFFDEB887
tan :: Colour
tan = Colour 0xFFD2B48C
rosyBrown :: Colour
rosyBrown = Colour 0xFFBC8F8F
sandyBrown :: Colour
sandyBrown = Colour 0xFFF4A460
goldenrod :: Colour
goldenrod = Colour 0xFFDAA520
darkGoldenrod :: Colour
darkGoldenrod = Colour 0xFFB8860B
peru :: Colour
peru = Colour 0xFFCD853F
chocolate :: Colour
chocolate = Colour 0xFFD2691E
saddleBrown :: Colour
saddleBrown = Colour 0xFF8B4513
sienna :: Colour
sienna = Colour 0xFFA0522D
brown :: Colour
brown = Colour 0xFFA52A2A
maroon :: Colour
maroon = Colour 0xFF800000
snow :: Colour
snow = Colour 0xFFFFFAFA
honeyDew :: Colour
honeyDew = Colour 0xFFF0FFF0
mintCream :: Colour
mintCream = Colour 0xFFF5FFFA
azure :: Colour
azure = Colour 0xFFF0FFFF
aliceBlue :: Colour
aliceBlue = Colour 0xFFF0F8FF
ghostWhite :: Colour
ghostWhite = Colour 0xFFF8F8FF
whiteSmoke :: Colour
whiteSmoke = Colour 0xFFF5F5F5
seaShell :: Colour
seaShell = Colour 0xFFFFF5EE
beige :: Colour
beige = Colour 0xFFF5F5DC
oldLace :: Colour
oldLace = Colour 0xFFFDF5E6
floralWhite :: Colour
floralWhite = Colour 0xFFFFFAF0
ivory :: Colour
ivory = Colour 0xFFFFFFF0
antiqueWhite :: Colour
antiqueWhite = Colour 0xFFFAEBD7
linen :: Colour
linen = Colour 0xFFFAF0E6
lavenderBlush :: Colour
lavenderBlush = Colour 0xFFFFF0F5
mistyRose :: Colour
mistyRose = Colour 0xFFFFE4E1
gainsboro :: Colour
gainsboro = Colour 0xFFDCDCDC
lightGray :: Colour
lightGray = Colour 0xFFD3D3D3
silver :: Colour
silver = Colour 0xFFC0C0C0
darkGray :: Colour
darkGray = Colour 0xFFA9A9A9
gray :: Colour
gray = Colour 0xFF808080
dimGray :: Colour
dimGray = Colour 0xFF696969
lightSlateGray :: Colour
lightSlateGray = Colour 0xFF778899
slateGray :: Colour
slateGray = Colour 0xFF708090
darkSlateGray :: Colour
darkSlateGray = Colour 0xFF2F4F4F
