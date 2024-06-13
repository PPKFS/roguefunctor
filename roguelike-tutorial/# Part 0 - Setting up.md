# Part 0 - Setting up

## Motivation

The original Python+libtcod roguebasin tutorial series is super popular and has been ported into many languages and frameworks over the years. "r/roguelikedev does the roguelike tutorial" has become a yearly event. There's a great roguelike fan Discord server with an active dev chat. Making roguelikes is great fun, and it's easier than people think. There's a kind of mental jump going between "I can solve all these problems" and "I can write an application". Maybe the reason these tutorial series are so popular, despite being very clear that nothing they are introducing is novel, is because they provide a welcoming helping hand. They take you from a bunch of classes to an actual *game* with a randomly generated dungeon with *monsters* and *combat* and *items* and all that cool stuff.

So, this is me attempting to do the same but for Haskell. A helping hand. Which is a bit of a challenge, given the reputation about how purity and functional programming isn't suited to gamedev, but it's far more doable than you think.

Fingers crossed this will be readable and followable in time for 2025!

## How much Haskell (and project organisation) do I need to know?

This tutorial does assume some degree of familiarity with Haskell (below) and cabal. Being able to add modules and dependencies and extensions to the `*.cabal` file, being able to run the game, being able to install a library.

Unfortunately, Haskell-wise, I think producing something like this for an audience who have never written a typeclass instance or don't know how `do` notation and `(>>=)` are related might be a bit of a challenge.

**However**, I'd hazard a guess that if you managed to get most of the way through any of Learn You A Haskell, Real World Haskell, any of the various other Haskell textbooks, your university functional programming course, and so on - you'll have no problems at all.

If the following *ideas* are at least familiar to you, then fingers are crossed that this should be understandable:
- The Haskell language
  - Datatypes, records, ADTs, polymorphism, typeclasses, constraints;
  - Functions and function composition;
  - `map`/`fmap`/`mapM`, `filter`, `fold`, and their friends;
- Monads
  - State, Reader, Writer, IO;
  - Monad stacks, in whatever form (concrete `StateT/ReaderT/WriterT` stacks, `mtl` typeclasses, effect systems, or just grokking the existence of multiple effects in one big monad)
  - do-notation

And it'd be *very useful* to know about:
- Lenses/Optics
  - Just acknowledging their existence as nicer ways to get, set, and modify nested record fields (*especially* in combination with state monads or stateful effects);
  - `^./use/view`, `.~/.=/set`, `%~/%=/over`
  - Using overloaded labels to avoid the pain of duplicate record field names or having `typenameFoo` everywhere;
- Effect systems - at least not running off at the sight of `State s :> es => Eff es a`! (spoilers: if you know `mtl`, this is simply `MonadState s m => m a` except you can have more than one in the same stack).
- Swapping out `Prelude` for a nicer version via `NoImplicitPrelude` - no more partial functions or having to use `String` everywhere (`Text` supremacy)
- Basic roguelike concepts like generating tile-based dungeons and an obsession of rendering things in ASCII characters, but that's why you're here - right?

Notably this won't include:
- ECS (Entity-component systems) - I don't like them, and I especially don't like them for roguelikes, and I especially especially don't like them for this kind of non-realtime Haskell gamedev.
- Doing things the simple way. Yes, it would probably be easier - for example - to not use `effectful` and just do everything in `StateT World IO`. But...I don't want to do that. I don't like using `mtl`. I want to use `effectful` and `optics` and a bunch of language extensions. I certainly will do my best to avoid unnecessarily complexity, but finishing a tutorial series with a monolithic gamestate object that is impossible to work on except by duct-taping more bells and whistles on top always annoyed me.

## Inspiration

This is a work heavily, heavily standing on the shoulders of giants. The structure of the current roguelike tutorials around are great. They're great. I just want to hope to port that greatness to Haskell. Specific mentions for:
- the original roguebasin tutorial;
- rogueliketutorials
- the bracket-lib/rltk tutorial (especially this last one).

Mostly this will follow the extended structure of the `bracket` tutorial in Rust because it has a whole bunch of extra content added on to the end of the first two. Whereas that tutorial leans heavily on leveraging an ECS, I'll instead be advocating for good ol' composition with some nice lens and phantom type solutions to keep type safety. So that'll be a change.

## Structure

- Part 0 (this one): Motivation, project setup, design decisions (why no ECS, a very brief introduction to `optics` and `effectful`)
- Part 1: Drawing a random map, moving around
- Part 2: Better map generation (rooms)
- Part 3: Field of view
- Part 4: Monsters, AI
- Part 5: Combat
- Part 6: UI
- Part 7: Items and inventory
- ???
