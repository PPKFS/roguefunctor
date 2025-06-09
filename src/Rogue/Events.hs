module Rogue.Events
  ( handleEvents
  , BlockingMode(..)
  , InputEvent(..)
  , makeEvent
  , checkEvent
  ) where

import Rogue.Prelude
import qualified Data.Set as S

data BlockingMode = Blocking | NotBlocking
  deriving stock (Eq, Ord, Generic, Show, Bounded, Enum)

handleEvents :: MonadIO m => BlockingMode -> (Keycode -> m a) -> m [a]
handleEvents bm f = do
  let allEvents :: MonadIO m => m [Keycode]
      allEvents = go True
        where
          go isEmptySoFar = do
            i <- terminalHasInput
            if i || (bm == Blocking && isEmptySoFar)
            then do
              r <- terminalRead
              (r :) <$> go False
            else
              pure []
  ev <- allEvents
  mapM f ev

data InputEvent =
  PressedKey Keycode
  | WithModifier InputEvent Keycode
  deriving stock (Eq, Show, Ord, Generic)

type ActiveModifierKeys = S.Set Keycode

checkEvent :: MonadIO m => ActiveModifierKeys -> InputEvent -> Keycode -> m Bool
checkEvent acMod ie kc = case ie of
  PressedKey k -> return (k == kc) &&^ allM (fmap (== 0) . terminalState) acMod
  k `WithModifier` s -> checkEvent acMod k kc &&^ fmap (== 1) (terminalState s)

makeEvent :: MonadIO m => ActiveModifierKeys -> Keycode -> m InputEvent
makeEvent acMod kc = do
  print acMod
  print kc
  mods <- mapMaybeM (\s -> (\case
    1 -> Just s
    _ -> Nothing) <$> terminalState s) $ S.toList acMod
  print mods
  case mods of
    [] -> return (PressedKey kc)
    x -> return $ foldl' WithModifier (PressedKey kc) x