# Part 1: Project setup, drawing an `@`, moving around

Let's get down to business

to defeat the Huns

by drawing an `@` to the screen in Haskell and using the WASD keys to move it around.

## Setting up

### Installing `bearlibterminal`

First, we need to download [`bearlibterminal`](https://github.com/cfyzium/bearlibterminal). There are prebuilt binaries available at the bottom of the readme. If you are on a platform that doesn't have a prebuilt binary (for example, an M1 Macbook with Apple Silicon) then you can build it yourself:

```bash

git clone https://github.com/cfyzium/bearlibterminal.git
cd bearlibterminal/Build
cmake ../
make all
```

With the library file (`.so`,`.dll`, `.dylib`) you have two options:

- you can either install this yourself into `/usr/lib` or equivalent.
- you can copy the library file into some directory of your project and pass the library option to cabal with `--extra-lib-dirs=/path/to/the/library`. The downside of this method is that you do *also* need to set `LD_LIBRARY_PATH`(Linux) or `DYLD_LIBRARY_PATH` (Mac) or something else (Windows) to actually do `cabal run`, because it doesn't copy the library into the cabal build directory. If anyone knows cabal better than I, please let me know how to set this up!

# Making a new `cabal` project

Let's begin by making a new project directory and initialising a blank `cabal` project.

```bash

mkdir hs-rogue
cd hs-rogue
cabal init
```

This will start the interactive wizard for setting up a new cabal project. The options of importance:

- we want just an executable project.
- we want **cabal version 3.4**, because `default-language: GHC2021` saves a lot of time with writing out language extensions.
- we want to use `GHC2021` as the language for our executable, as this enables a bunch of language extensions for us.

This gives us `hs-rogue.cabal` pre-populated. Unfortunately, the libraries we need `bearlibterminal-hs` and `roguefunctor` are not on Hackage so cabal cannot automatically download them. We need a `cabal.project` file that specifies where the repositories for these libraries can be found as well as the packages in our project. We only have one package, but otherwise `cabal run` will complain that the project file lists no packages to run. Create `cabal.project` and add the following:

```cabal
source-repository-package
  type: git
  location: https://github.com/PPKFS/roguefunctor.git
  tag: b2de932cffea43767df772582aea52c62d1843b3
source-repository-package
  type: git
  location: https://github.com/PPKFS/bearlibterminal-hs.git
  tag: c6684515801a500655e30c14d787dbb46e2e0529
packages:
  hs-rogue.cabal
```

Whilst we're at it, it's safe to assume we would like `haskell-language-server` to work with this project so we'll add `hie.yaml` as well:

```yaml
cradle:
  cabal:
    - path: "hs-rogue/app"
      component: "executable:hs-rogue"
```

So now we will have tooltips and code actions and hints. Nice.

We've got one final setup step before we can get on with the drawing, and that's adding some dependencies and extensions to the `cabal` file. I'll make sure in future parts to put all the modifications to the `.cabal` file at the start of the post. Under `build-depends`, add:

```cabal
build-depends:
  base
  , bearlibterminal
  , roguefunctor
  , containers
  , optics
```

Now if you run `cabal build`, it should download and build the two libraries. You'll know your setup for installing `bearlibterminal` was correct if it successfully builds, as otherwise it will give errors about missing C libraries.

Finally we can add a few default extensions:
```cabal
    default-extensions:
      NoImplicitPrelude
      LambdaCase
      OverloadedLabels
      OverloadedStrings
      DerivingStrategies
```

- `NoImplicitPrelude` works better than cabal mixins for using a custom prelude.
- `LambdaCase` is just a no-brainer because `\case` is great.
- `OverloadedLabels` is needed for making `optics` super nice to work with.
- `OverloadedStrings` because we want to use `Text` over `String` for efficiency.
- `DerivingStrategies` is just good practice.

## Drawing an `@`

Without further ado, we can draw things.

```haskell


import Rogue.Prelude
import Rogue.Window
import Rogue.Config
import Rogue.Geometry.V2
import Rogue.Colour
import Rogue.Events

screenSize :: V2
screenSize = V2 100 50

main :: IO ()
main =
  withWindow
  defaultWindowOptions { size = Just screenSize }
  pass
  (const runLoop)
  pass

runLoop :: MonadIO m => m ()
runLoop = do
  -- we have no update logic currently
  -- rendering
  terminalClear
  terminalColour (fromRGB 255 128 255)
  void $ terminalPrintText 10 10 "@"
  terminalRefresh
  -- event handling
  shouldContinue <- handleEvents Blocking $ \case
    WindowEvent Resize -> return True
    WindowEvent WindowClose -> return False
    Keypress TkEsc -> return False
    _ -> return False
  when (and shouldContinue) runLoop
```

If you `cabal run hs-rogue` this, you'll get a black screen with a magenta `@` on it. Hooray.

As I've previously said, I am assuming a degree of Haskell proficiency for the tutorial so I'll not step through with "this is a function, this is a constraint, this is what the `$` operator does" and so forth.

### Window handling

You can draw a window in two ways. One is to use the functions exposed directly from `bearlibterminal` - `terminalOpen :: MonadIO m => m Bool` and `terminalClose`. The [documentation for the original library](http://foo.wyrd.name/en:bearlibterminal:reference#open) explains things in slightly more detail. As a rule of thumb, the Haskell bindings for `bearlibterminal` are basically identical to the original C/C++ except with wrappers around `int`s (as `Bool` or some enum ADT).

The other recommended way is `withWindow :: MonadUnliftIO m => WindowOptions -> m a -> (a -> m b) -> m c -> m b`. This is a wrapper around `bracket_` that takes some initialisation function, some main loop, and some cleanup function and makes sure that we avoid memory leaks by closing the window if an error occurs. In our case, we have no additional logic for either setup or cleanup (opening and closing the terminal is handled automatically) so we pass `pass` (`pass = return ()` from `relude`. It's super handy).

We follow the standard game loop pattern of doing any per-turn (or per-frame) logic, rendering, and then handling any events. We currently don't have any logic. For rendering, we clear the screen before drawing our pink `@` and then refresh the window.

### Event handling

For event handling, we once again can use the raw `bearlibterminal` event handling with `terminalRead` and `terminalHasInput` or the `handleEvents` helper. `handleEvents` will clear all pending events - keypresses, window events, etc - and map over each of them. It can also either ensure there is at least one event (`Blocking`) in the queue or not (`NonBlocking`). As we're doing a traditional turn-based game, we'll handle it with `Blocking`.

A quirk of the library is that the first event will always be a window resize event, which we want to ignore. If we receive a `WindowClose` event (either pressing the `X` or sending a `SIGTERM/SIGKILL`) then we stop and quit, otherwise we run the loop again. We also close the window on `Esc` for ease. Note that because `handleEvents` may handle more than 1 event at once, we get a list of results and we only continue the loop if *none* of them are `WindowClose`.

## Moving around

Now let's add some movement handling. For now, we aren't going to do some sort of `Action` hierarchy for different kinds of actions (this will be a lot later) or setting up game objects (we'll do that when we add a map in the next part).

We want to separate the actual keypresses from the directions we want to move from the moving logic:

```haskell
import qualified Data.Map as M

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

```

Here we use `optics` to give us lenses to access the first and second components of the vector with `_1` and `_2` respectively. Normally these are the lenses for elements of a tuple, but because we have `instance Field1/Field2` for `V2` we can use `_1` and `_2` for it as well. Neat.

Now we need to track the player's position in a `State` effect. Whilst we're at it, we'll also keep track of whether we should quit the game to tidy up the event handling. We'll have to rewrite our loop to be in the `Eff` monad but fortunately, this is pretty easy.

```haskell
initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

data WorldState = WorldState
  { playerPosition :: V2
  , pendingQuit :: Bool
  } deriving stock (Generic)

runLoop :: (IOE :> es, State WorldState :> es) => Eff es ()
runLoop = do
  terminalClear
  terminalColour (fromRGB 255 128 255)
  playerPos <- gets playerPosition
  withV2 playerPos terminalPrintText "@"
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

```

We update `runLoop` to now be in `Eff`, with [at least] two effects available to us: IO, with the `IOE` effect; and mutable state with the `State` effect. There could be more in `es` but if there are, we can't utilise them. Perhaps the `bearlibterminal` calls should be in their own effect, so that way we don't need `IO` explicitly and can't fire the missiles.

We now fetch the player position from the state and use that position to render the `@`. `withV2 :: V2 -> (Int -> Int -> r) -> r` is handy for automatically splitting out the components of a vector.

As we now have state, we rewrite the event handling loop to instead update that we should close the program at the next opportunity. `OverloadedLabels` and `optics` makes it very nice and clean to do.

If we press a key that maps to `Just direction` (one of WASD or the arrows), then we modify the player position.

The last thing we need to do is to call our effect handlers; this is a composition of functions that will evaluate one layer of the `es :: [Effect]` list until we have handled them all and we finish with `runPureEff` to evaluate to a pure value or `runEff` to evaluate to `IO a`.

```haskell

import Effectful.State.Static.Local

main :: IO ()
main = runEff $
  evalState (WorldState initialPlayerPosition False) $
    withWindow
    defaultWindowOptions { size = Just screenSize }
    pass
    (const runLoop)
    pass
```

Right now we only have 1 (2, including IO) effect so we just need to interpret the state effect and then use `runEff` to go back to `IO ()`.

Now when you run it, you can move around!

# Wrapping up

That's it for part 1. We now have something drawn and we can move around.