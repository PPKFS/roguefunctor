# Part 0 - Setting up

# Motivation and introduction

The original Python+libtcod roguebasin tutorial series is super popular and has been ported into many languages and frameworks over the years. "r/roguelikedev does the roguelike tutorial" has become a yearly event. There's a great roguelike fan Discord server with an active dev chat. Making roguelikes is great fun, and it's easier than people think. There's a kind of mental jump going between "I can solve all these problems" and "I can write an application". Maybe the reason these tutorial series are so popular, despite being very clear that nothing they are introducing is novel, is because they provide a welcoming helping hand. They take you from a bunch of classes to an actual *game* with a randomly generated dungeon with *monsters* and *combat* and *items* and all that cool stuff.

So, this is me attempting to do the same but for Haskell. A helping hand. Which is a bit of a challenge, given the reputation about how purity and functional programming isn't suited to gamedev, but it's far more doable than you think.

Fingers crossed this will be readable and followable in time for 2024!

# How much Haskell (and project organisation) do I need to know?

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

# I want to get to the actual tutorial already!

## Part 1 where the actual project starts can be found here.

Otherwise, read on for some more mulling over implementation details and a brief explanation of `optics` and `effectful`.

## Inspiration

This is a work heavily, heavily standing on the shoulders of giants. The structure of the current roguelike tutorials around are great. They're great. I just want to hope to port that greatness to Haskell. Specific mentions for:
- the original roguebasin tutorial;
- rogueliketutorials
- the bracket-lib/rltk tutorial (especially this last one).

Mostly this will follow the extended structure of the `bracket` tutorial in Rust because it has a whole bunch of extra content added on to the end of the first two. Whereas that tutorial leans heavily on leveraging an ECS, I'll instead be advocating for good ol' composition with some nice lens and phantom type solutions to keep type safety. So that'll be a change.

## Structure

- Part 0 (this one): Motivation, design decisions (why no ECS, a very brief introduction to `optics` and `effectful`), project setup, drawing an `@`
- Part 1: Drawing a random map, moving around
- Part 2: Better map generation (rooms)
- Part 3: Field of view
- Part 4: Monsters, AI
- Part 5: Combat
- Part 6: UI
- Part 7: Items and inventory
- ???

# Why no ECS?

tl;dr: I don't think it's particularly necessary in roguelikes, or quite so much in Haskell, or definitely in Haskell roguelikes.

I think you could probably ignore this section happily and just continue to the next section of the tutorial after that tl;dr. But if you would like to read a ramble, read on.

There's a great video on the topic here:

The long answer:
  So first, let's all get on the same page over what an entity-component system *is*:

- *Entities*, which are thin wrappers around some sort of ID. This is as collated as an object gets.

- *Components*, which are a collection of data associated with a given *entity*, usually as a bag. An Entity may (and probably will) have many components. Examples would be a controllable component, a position, a renderable representation, and so on.

- *Systems* are the logic that act on components. They take the form of iterating over each component of a certain type, or a collection of types, and perform the logic. Examples would be a system that accepts input for each controllable component, or a system that iterates entities with **both** position and renderable components and renders them at a location.

This has many advantages. Each system is compartmentalised; it does one thing and one thing only. It's clear that every entity the AI system touches would have some reason to be controlled by the AI system. There is no mess of OOP multiple inheritance possible, because everything is the loosest possible form of composition - and you can compose dynamically at runtime (adding or removing components on the fly).

There's a great ECS library for Haskell, too - Apecs.

So why am I not using one? ECS systems move the data model from having **objects** to having **bags of components**. I don't like thinking of objects as loose bags of components. Objects are not loose bags of (un)related components.

Let's take some goblins. Each of these goblins consist of a renderable (a red `g` for example), some combat stats (HP, attack, etc), an inventory, an AI behaviour, and some spatial location in the dungeon. In an ECS, there is no "goblin". There are 10 goblin IDs, there are 10 combat components (each associated with an entity), 10 position components, etc. - and you can probably gather all the components for a given ID to get a bag of goblin components.

But you don't have the *guarantees* that every goblin will have these things! What if a goblin doesn't have a renderable component? You just have to assume that the bag-of-components contains a renderable. You never need to think about the renderable part of a specific goblin because your interaction with that component will almost always be as part of rendering everything.

Let's say you now want to make a goblin archer. In an OOP approach you'd do something with inheritance, and you may get the dreaded diagonal inheritance problem if you try to inherit from goblin AND archer..and ECS solves this fairly easily, because you can just add the union of components between the two things. But now we're at the stage where we have *even more various components floating around to keep track of*! And we need stores for each of these components, and so on.

Let's consider my Haskell solution, where objects are simply an `Object` type. We have

```haskell

data Object = Object
  { name :: Text
  , objectId :: Entity
  , position :: Position
  , renderable :: Renderable
  , ...
  , objectSpecifics :: ObjectSpecifics
  }

data ObjectSpecifics =
  GoblinSpecifics GoblinData
  | GoblinArcherSpecifics GoblinArcherData
  -- we don't need these because we only need a `Specifics` for a concrete object!
  -- | MonsterSpecifics MonsterData
  -- | ArcherSpecifics ArcherData

data GoblinData = GD
  { specificAiKind :: SomeAIInfo
  , combat :: CombatStats
  , ...
  }

data GoblinArcherData = GAD
  { goblinInfo :: GoblinData
  , archerInfo :: ArcherData
  }

...and so on
```

Simple, nice, clean Haskell. Every object has some generic properties that every object has (like a renderable) and then our specific instances are just a big sum type. Easy.

"Ah, but how can I iterate over every AI-controlled object in my system?" Well, we have lenses for that! With a little magic, we can write fairly simple instances that give us functions of the form `getAIMaybe :: Object -> Maybe SomeAIInfo` (no matter where the `SomeAIInfo` is placed in the composition), and `setAI :: Object -> SomeAIInfo -> Object`. This is no more than doing, paraphrased:

```haskell

-- this is an AffineTraversal, i.e. a traversal with 0-1 targets, or a lens plus a prism, or a prism that doesn't have the reverse condition
instance HasSpecifics ObjectSpecifics SomeAIInfo where
  propertyAT = (_GoblinSpecifics % #specificAiKind) `thenATraverse` (_GoblinArcherSpecifics % #goblinInfo %specificAiKind)
```

Okay, it's a little boilerplate but it's not so bad!

"Ah, but what about if we want to have type safety? What if I *know* this thing is a goblin and I want to avoid the `Maybe`ness?"

We can introduce some witnesses for this!

```haskell

-- given the existence of a goblin component for this entity, and this entity...

instance TaggedEntity GoblinData GoblinEntity
-- giving us tagEntity :: GoblinData -> Entity -> GoblinEntity
-- and also getGoblinData :: GoblinEntity -> Eff es GoblinData
```
If we've tagged an Entity with proof it has some property, we can now look that property up safely. It also means we can safely store references in a type-safe way. Perhaps a goblin has parents, which can be fields of `GoblinEntity` rather than just `Entity`. We can no longer set a goblin's parents to be a potion of health...

If you've read this far, I hope this was marginally understandable! Much of it could *probably* be done in an ECS. Much of it probably *should* be done in an ECS. But because **I want to write logic on objects rather than on components**, I like this system of simple composition plus some clever lenses and type witnesses.

# ECS ramble over, the world's quickest introduction to `optics` and `effectful`

## `optics`

Optics and lenses are a solution to Haskell's record problem. As most Haskellers know, trying to modify a record field can be annoying:

```haskell

data R2 = R2 { nestedField :: Int } deriving Generic
data Record = R { someField :: R2 } deriving Generic
modifyIt :: R -> (Int -> Int) -> R
modifyIt record f =
  let f1 = someField record
  in record { someField = f1 { nestedField = f (nestedField f1) } }
```

and so can a nested lookup with some logic:

```haskell
lookupNestedMap :: Record -> Key -> (Value -> b) -> Maybe b
lookupNestedMap record k f = fmap f . lookup k . field3WhichIsAMap . field2 . field1 $ record
```

Yuck. Fortunately, there exists a solution in the form of optics. An optic is a combination of "here is how to look up the target(s) associated with this optic, and here is how to modify the target(s) associated with this optic". I say "target(s)", because this can differ on the optic. 95% of the time, you'll just need `Lens` and `Prism`.

- A `Lens'` is a 1-1 relationship. It's basically a record field. You can get the field and set the field.
- A `Prism'` is a 0-1 relationship (with some additional reconstruction properties I won't get into). It's basically `Maybe` a record field. You can *maybe* get the field, you can set the field *if it exists*. Yes, set is `fmap`.

And when it comes to doing things with optics, there's three main things:

- you can `view` the target, `view theLens x` or `x ^. theLens`.
- you can `set` the target, `set theLens newValue x` or `x & theLens .~ newValue`.
- you can apply a function to (`over`) the target, `over theLens f x` or `x & theLens %~ f`.

The order of arguments means that you can sequence operations to perform multiple `set` and `over` on something by using reverse function composition `(&)`:

```haskell

x & lens1 .~ val1 & lens2 %~ f2 & lens3 .~ val3

-- is the same as

let x1 = x & lens1 .~ val1
    x2 = x1 & lens2 %~ f2
    in
      x2 & lens3 .~ val3
```

Optics can be composed. If you have a lens from `s` to `a`, and a lens from `a` to `z`, you can make a lens from `a` to `z`. For the `lens` library this is `.` (same as function composition) and for `optics` it's `%`.

The first example can be written with `optics` as

```haskell

modifyIt record f = record & (#someField % #nestedField %~ f)
```

and the second as

```haskell
-- ^? is roughly "view a maybe value"

lookupNestedMap record k f = f <$> (record ^? #field1 % #field2 % #field3WhichIsAMap % at k)
```

Much nicer!

Optics (the concept) are mostly implemented by two big libraries, `lens` and `optics` (the library) as well as some others (e.g. `microlens`). Both differ slightly in implementation, scope, and so on and a discussion is out of scope and also my wheelhouse here but I prefer to use `optics` for:

- it supports making lenses with `OverloadedLabels` from a `Generic` instance
- the type errors are nicer
- easier support for adding extra label optics

For a better introduction, check out:

- [School of Haskell's introduction to lens](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
- [the tutorial for `lens`](https://hackage.haskell.org/package/lens-tutorial-1.0.5/docs/Control-Lens-Tutorial.html) (but swapping `%` and `.` and `lens` has a `Functor` based implementation)
- [documentation for `optics`](https://hackage.haskell.org/package/optics-0.4.2.1/docs/Optics.html)

## effectful

[`effectful`](https://github.com/haskell-effectful/effectful) is an algebraic effects library for Haskell. If you've used `mtl` or `transformers`, it's very similar. You want a set of functionality in your monad environment. For this example, we want the ability to do IO, a reader over an environment, a state over a `WorldState`, and some custom other monad. Using a concrete transformer stack:

```haskell

doThings :: ReaderT Environment (StateT WorldState (SomeOtherMonad IO)) a
doThings = do
  env <- ask
  modify updateWorld
  ...
```

or using `mtl` constraints

```haskell
doThings :: (MonadIO m, MonadReader Environment m, MonadState WorldState, MonadSomethingElse m) => m a
doThings = do
  env <- ask
  liftIO $ print "to the console"
  modify updateWorld
  ...
```

And using `effectful` this is almost the same. Rather than some monad `m`, we work in the `Eff es` monad. Rather than constraints over the monad, we parameterise `Eff` with `es :: [Effect]` - the list of effects we want available.

```haskell
doThings :: (IOE :> es, Reader Environment :> es, State WorldState :> es, SomethingElse :> es) => Eff es a
doThings = do
  env <- ask
  liftIO $ print "to the console"
  modify updateWorld
  ...
```

And that's it. It's very similar to `mtl`. Rather than talking of monad transformers, we talk of effects. It has plenty of advantages, most of which I won't get into here, but the big ones are:

- the implementation of the `State` effect runs in mutable state behind the scenes, which is significantly faster.
- you can have multiple effects of the same kind at the same time. In `mtl`, you can't have multiple `MonadState` constraints but `effectful` allows you to have `State a :> es, State b :> es` with visible type applications at the use site.
- thread-local and shared versions of `State`
- writing your own effects (e.g. nicer, domain-specific versions of state/reader) is simple. No messing about with writing endless amounts of lifting instances or newtype wrappers.

