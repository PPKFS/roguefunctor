module Rogue.Window
  ( initWindow
  , withWindow

  ) where
import Rogue.Prelude
import Rogue.Config
import Control.Monad.Catch
import Rogue.Monad
import System.Random.Stateful

initWindow :: MonadIO m => WindowOptions -> m ()
initWindow opts = do
  void $ terminalOpen
  void $ terminalSetOptions opts

withWindow :: HasCallStack => MonadMask m => MonadIO m => WindowOptions -> RogueT m a -> (a -> RogueT m b) -> RogueT m c -> m b
withWindow opts initialise loop exit = bracket
  (do
    initWindow opts
    g <- initStdGen
    let gen = RogueState (AtomicGen g) 0
    runStateT (runRogueT initialise) gen)
  (\(_, rs) -> evalStateT (runRogueT $ exit >> terminalClose) rs)
  (\(a, rs) -> evalStateT (runRogueT $ loop a) rs)
