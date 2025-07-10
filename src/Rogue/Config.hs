{-# LANGUAGE RecordWildCards #-}
module Rogue.Config
  ( BearLibConfigString(..)
  , WindowOptions(..)
  , TrueTypeFontOptions(..)
  , TTFHinting(..)
  , TTFMode(..)
  , terminalSetOptions
  , terminalSetOptions_
  , defaultWindowOptions
  ) where

import Rogue.Prelude
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy as TL
import qualified Data.List as L
import qualified Data.Text as T

class BearLibConfigString s where
  toConfigString :: s -> LT.Builder

data Cellsize = Auto | Size (Int, Int)
  deriving stock (Eq, Ord, Show, Generic)

instance BearLibConfigString Int where
  toConfigString = LT.fromString . show

instance BearLibConfigString Char where
  toConfigString = LT.fromString . one

instance BearLibConfigString (Int, Int) where
  toConfigString (x, y) = LT.fromString (show x) <> LT.singleton 'x' <> LT.fromString (show y)

instance BearLibConfigString V2 where
  toConfigString (WithV2 x y) = LT.fromString (show x) <> LT.singleton 'x' <> LT.fromString (show y)

instance BearLibConfigString Cellsize where
  toConfigString Auto = LT.fromText "auto"
  toConfigString (Size s) = toConfigString s

instance BearLibConfigString Text where
  toConfigString s = LT.singleton '"' <> LT.fromText (T.replace "\"" "\"\"" s) <> LT.singleton '"'

instance BearLibConfigString String where
  toConfigString s = LT.singleton '"' <> LT.fromText (T.replace "\"" "\"\"" $ T.pack s) <> LT.singleton '"'

instance BearLibConfigString Bool where
  toConfigString True = LT.fromText "true"
  toConfigString False = LT.fromText "false"

newtype ConfigOption = ConfigOption { unConfig :: (Text, LT.Builder) }

instance BearLibConfigString ConfigOption where
  toConfigString (ConfigOption (t, v)) = LT.fromText t <> (if t == "" then mempty else LT.singleton '=') <> v

toByteString :: BearLibConfigString c => c -> BS.ByteString
toByteString = BS.toStrict . LT.encodeUtf8 . LT.toLazyText . toConfigString

terminalSetOptions :: MonadIO m => BearLibConfigString c => c -> m Bool
terminalSetOptions = terminalSet . TL.toStrict . LT.toLazyText . toConfigString

terminalSetOptions_ :: MonadIO m => BearLibConfigString c => c -> m ()
terminalSetOptions_ = void . terminalSet . TL.toStrict . LT.toLazyText . toConfigString

makeOptions :: Text -> [Maybe ConfigOption] -> LT.Builder
makeOptions main c = let options = map toConfigString . catMaybes $ c in
  case options of
    [] -> mempty
    opts -> LT.fromText (main <> ": ") <> mconcat (L.intersperse (LT.singleton ',') opts) <> LT.singleton ';'

maybeToOption :: Functor f => BearLibConfigString g => Text -> f g -> f ConfigOption
maybeToOption t = fmap (ConfigOption . (t,) . toConfigString)

data WindowOptions = WindowOptions
  { size :: Maybe V2
  , cellsize :: Maybe Cellsize
  , title :: Maybe Text
  , icon :: Maybe FilePath
  , resizeable :: Maybe Bool
  , fullscreen :: Maybe Bool
  } deriving stock (Show, Eq, Ord, Generic)

defaultWindowOptions :: WindowOptions
defaultWindowOptions = WindowOptions
  { size = Nothing
  , cellsize = Nothing
  , title = Nothing
  , icon = Nothing
  , resizeable = Nothing
  , fullscreen = Nothing
  }

instance BearLibConfigString WindowOptions where
  toConfigString WindowOptions{..} =
    makeOptions "window"
      [ maybeToOption "size" size
      , maybeToOption "cellsize" cellsize
      , maybeToOption "title" title
      -- todo: work out how filepaths should work
      -- todo: this should probably be done with generics
      , maybeToOption "icon" icon
      , maybeToOption "resizeable" resizeable
      , maybeToOption "fullscreen" fullscreen
      ]


data TTFMode = Monochrome | NormalRasterisation | LCD
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)

data TTFHinting = NormalHinting | AutoHint | NoHinting
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)

instance BearLibConfigString TTFMode where
  toConfigString = \case
    Monochrome -> "monochrome"
    NormalRasterisation -> "normal"
    LCD -> "lcd"

instance BearLibConfigString TTFHinting where
    toConfigString = \case
      NormalHinting -> "normal"
      AutoHint -> "autohint"
      NoHinting -> "none"

data TrueTypeFontOptions = TrueTypeFontOptions
  { font :: FilePath
  , size :: V2
  , sizeReference :: Maybe Char
  , mode :: Maybe TTFMode
  , codepage :: Maybe Int
  , spacing :: Maybe V2
  , useBoxDrawing :: Maybe Bool
  , useBlockElements :: Maybe Bool
  , hinting :: Maybe TTFHinting
  } deriving stock (Show, Eq, Ord, Generic)

instance BearLibConfigString TrueTypeFontOptions where
  toConfigString TrueTypeFontOptions{..} =
    makeOptions "font"
      [ maybeToOption "" (Just font)
      , maybeToOption "size" (Just size)
      , maybeToOption "size-reference" sizeReference
      , maybeToOption "mode" mode
      , maybeToOption "codepage" codepage
      , maybeToOption "spacing" spacing
      , maybeToOption "use-box-drawing" useBoxDrawing
      , maybeToOption "use-block-elements" useBlockElements
      , maybeToOption "hinting" hinting
      ]