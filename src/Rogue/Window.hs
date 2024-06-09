module Rogue.Window
  ( initWindow
  , withWindow

  ) where
import Rogue.Prelude
import Rogue.Config
import UnliftIO (bracket)

initWindow :: MonadIO m => WindowOptions -> m ()
initWindow opts = do
  void $ terminalOpen
  void $ terminalSet opts

withWindow :: MonadUnliftIO m => WindowOptions -> m a -> (a -> m b) -> m c -> m b
withWindow opts initialise loop exit = bracket
  (initWindow opts >> initialise)
  (const $ exit >> terminalClose)
  loop
