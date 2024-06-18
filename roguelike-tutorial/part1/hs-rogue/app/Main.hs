module Main where

import Rogue.Prelude
import Rogue.Window
import Rogue.Config
import Rogue.Geometry.V2
import Rogue.Colour
import Rogue.Events
import qualified Data.Map as M
import Effectful.State.Static.Local

screenSize :: V2
screenSize = V2 100 50

initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

data WorldState = WorldState
  { playerPosition :: V2
  , pendingQuit :: Bool
  } deriving stock (Generic)

main :: IO ()
main = runEff $
  evalState (WorldState initialPlayerPosition False) $
    withWindow
    defaultWindowOptions { size = Just screenSize }
    pass
    (const runLoop)
    pass

data Direction = LeftDir | RightDir | UpDir | DownDir
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

movementKeys :: M.Map Keycode Direction
movementKeys = M.fromList
  [ (TkA, LeftDir)
  , (TkS, DownDir)
  , (TkW, UpDir)
  , (TkD, RightDir)
  , (TkUp, UpDir)
  , (TkDown, DownDir)
  , (TkLeft, LeftDir)
  , (TkRight, RightDir)
  ]

asMovement :: Keycode -> Maybe Direction
asMovement k = k `M.lookup` movementKeys

calculateNewLocation :: Direction -> V2 -> V2
calculateNewLocation dir =
  case dir of
    LeftDir -> _1 %~ subtract 1
    RightDir -> _1 %~ (+1)
    UpDir -> _2 %~ subtract 1
    DownDir -> _2 %~ (+1)

runLoop :: (IOE :> es, State WorldState :> es) => Eff es ()
runLoop = do
  terminalClear
  terminalColour (fromRGB 255 128 255)
  playerPos <- gets playerPosition
  void $ withV2 playerPos terminalPrintText "@"
  terminalRefresh
  void $ handleEvents Blocking $ \case
    WindowEvent WindowClose -> modify (#pendingQuit .~ True)
    Keypress TkEsc -> modify (#pendingQuit .~ True)
    Keypress other -> case asMovement other of
      Just dir -> modify (#playerPosition %~ calculateNewLocation dir)
      Nothing -> pass
    _ -> pass
  shouldContinue <- not <$> gets pendingQuit
  when shouldContinue runLoop