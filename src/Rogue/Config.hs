{-# LANGUAGE RecordWildCards #-}
module Rogue.Config
  ( BearLibConfigString(..)
  , WindowOptions(..)
  , terminalSetOptions
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
  toConfigString (ConfigOption (t, v)) = LT.fromText t <> LT.singleton '=' <> v

toByteString :: BearLibConfigString c => c -> BS.ByteString
toByteString = BS.toStrict . LT.encodeUtf8 . LT.toLazyText . toConfigString

terminalSetOptions :: MonadIO m => BearLibConfigString c => c -> m Bool
terminalSetOptions = terminalSet . TL.toStrict . LT.toLazyText . toConfigString

data WindowOptions = WindowOptions
  { size :: Maybe V2
  , cellsize :: Maybe Cellsize
  , title :: Maybe Text
  , icon :: Maybe FilePath
  , resizeable :: Maybe Bool
  , fullscreen :: Maybe Bool
  } deriving stock (Show, Eq, Ord)

defaultWindowOptions :: WindowOptions
defaultWindowOptions = WindowOptions
  { size = Just $ V2 150 50
  , cellsize = Just Auto
  , title = Just "Hello Zurihac!!!"
  , icon = Nothing
  , resizeable = Just False
  , fullscreen = Just False
  }

instance BearLibConfigString WindowOptions where
  toConfigString WindowOptions{..} =
    let f :: Functor f => BearLibConfigString g => Text -> f g -> f ConfigOption
        f t = fmap (ConfigOption . (t,) . toConfigString)
        mkOptions = map toConfigString $ catMaybes
          [ f "size" size
          , f "cellsize" cellsize
          , f "title" title
          -- todo: work out how filepaths should work
          -- todo: this should probably be done with generics
          , f "icon" icon
          , f "resizeable" resizeable
          , f "fullscreen" fullscreen
          ]
    in
      case mkOptions of
        [] -> mempty
        opts -> LT.fromText "window: " <> mconcat (L.intersperse (LT.singleton ',') opts) <> LT.singleton ';'
