Chapter 13: `do`ing stuff
======

First things first: clone the example project, open stack.yaml, and let's
upgrade the GHC version the project is going to use. Maybe this doesn't
make a difference for you, but for me, running on 5.14, Intero wasn't
working, and Intero is really important for me.

Set:

```yaml
resolver: lts-9.4
```

13.2 Making packages with Stack
------

We're going to use two tools pretty heavily:

- Cabal (Common Architecture for Building Applications and Libraries): this is the config file for the package. It describes what executables might be contained, what the dependencies are, and where code lives
- Stack: this is the build tool. It understands the config and knows how to turn source code into compiled packages

13.3 Working with a basic project
-----

Some basic stack commands:

This compiles the project and dumps an executable somewhere stack knows about:

```bash
stack build
```

This compiles a project and makes stack watch our project for changes (they didn't tell us about this one but I don't like typing and maybe you don't either):

```bash
stack build --file-watch
```

This runs a compiled program, in this case, the `hello` program:

```bash
stack exec -- hello
```

This bootstraps the stack environment:

```bash
stack setup
```

### Executable stanzas

This is ours from `hello.cabal`:

```
executable hello
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
```

It tells stack where source code lives, what the main function is, what language to use (which is apparently boring boilerplate), and what the build dependencies are. Normally our haskell project will depend on more than just haskell base.

13.4 Making our project a library
-----

In case we want a reusable `hello` command line tool.

To make a library, we add a library stanza that looks a lot like the executable stanza, but instead of specifying a `Main`, it specifies what becomes importable.

```
library
  hs-source-dirs:      src
  exposed-modules:     Hello
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
```

Then we can make a `Hello.hs` in `src/` and import `sayHello` from there in our `Main.hs`.

This works because `Hello.hs` is in `src`, which our executable stanza knows about. If we had put `Hello.hs` somewhere else, we would have had to make the executable depend on the `hello` library, like this:

```
executable hello
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
                       hello
```

13.5 Module exports
-----

We don't have to export every top-level thing from a module if we don't want to. We can't mark things private, but we _can_ indicate that only some things get exported, with

```haskell
module Foo
  ( thisCanBeImportedElsewhere )
  where
...
-- this can't
privateMethod :: a -> b
privateMethod = undefined
```

Specifying an empty export list makes the compiler sad, so don't do it.

13.6 Importing Modules
-----

Import order doesn't matter -- anything imported is in scope for the entire module, regardless of where/when it's imported.

To browse a module, use `:browse` in the repl --

```haskell
Prelude> :browse Data.Bool
Data.Bool.bool :: a -> a -> Bool -> a
(&&) :: Bool -> Bool -> Bool
data Bool = False | True
not :: Bool -> Bool
otherwise :: Bool
(||) :: Bool -> Bool -> Bool
```

To run a repl without the prelude, run `stack ghci --ghci-options -XNoImplicitPrelude` outside a project (maybe it will override options for the project or something if you do it inside a project? Risk isn't made clear).

Then we can import specific functions from modules with `import Module.Thing (foo)`, e.g.:

```haskell
Prelude> import Data.Bool (bool)
```

### Qualified Imports

Qualified imports let us keep module references when we want functions from them. This is useful if we have name collisions.

```haskell
Prelude> import qualified Data.Bool
```

^^ requires us to refer to all functions contained in `Data.Bool` as
`Data.Bool.foo`

We can also alias at the same time with a qualified import:

```haskell
Prelude> import qualified Data.Bool as B
```

#### Setting the Prelude prompt

With many imports, your prompt will keep growing, so use `:set` to set it to
something specific, e.g.:

```haskell
Prelude Data.Bool Data.Maybe> :set prompt "Foo> "
Foo> 
```

### Intermission

1. Functions being imported from `Control.Mondad`? `forever`, `when`
2. Which imports are unqualified and imported in their entirety? `Database.Blacktip.Types`, `Data.Bits`
3. From the name, what do you suppose importing `Types` from `Blacktip` brings in? The types necessary in `blacktip` for talking to databases
4. What's going on in the code:
  1. The type signature refers to three aliased imports. What are the modules behind the aliases? `Control.Concurrent.MVar`, `Filesystem.Path.CurrentOS`, `ControlConcurrent`
  2. Which import does `FS.writeFile` refer to? `Filesystem`
  3. Which import did `forever` come from? `Control.Monad`

13.7 Making our program interactive
-----

We get input from the user with `getLine`. When we make `sayHello` take a `String` argument, its type becomes `String -> IO ()`

```haskell
sayHello :: String -> IO ()
sayHello name = putStrLn $ "hello " + name + " world"
```

Then the `main` function becomes

```haskell
main = do
  name <- getLine
  sayHello name
```

`do` "sequence[s] side-effects in a convenient syntax" -- 

- `getLine` is the first `IO` that needs to happen

```haskell
Prelude> :t getLine
getLine :: IO String
```

- followed by `sayHello`
- followed by `dogs`

`stack build`, `stack exec -- hello`, then throw in a name, and by magic we can greet specific people now

We can't pass `getLine` to `sayHello` because the type is wrong -- `do` does some magic to unwrap the inner type.

##### The magic, even though we don't know what some of the words mean yet

`do`:

> Syntactic sugar for use with monadic expressions. For example:
>
>  do { x ; result <- y ; foo result }
>  is shorthand for:
>
>   x >> 
>   y >>= \result -> foo result

Where:

```haskell
Prelude> :i >>
class Applicative m => Monad (m :: * -> *) where
  ...
  (>>) :: m a -> m b -> m b
  ...
  	-- Defined in ‘GHC.Base’
infixl 1 >>
```

```haskell
Prelude> :i >>=
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  ...
  	-- Defined in ‘GHC.Base’
infixl 1 >>=
```

So those are available on everything with a `Monad m` instance, where `m` has an instance of the `Applicative` typeclass. As they say, _later_.

### Adding a prompt

Our program would be more ergonomic if the user knew the program was waiting for input, so

- add a `System.IO` import
- set `stdout` buffering to none and throw a prompt out there

```haskell
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please input your name: "
  ...
```

Note -- not `putStrLn` because we don't want the line break, for _reasons_

Somehow the `hSetBuffering stdout NoBuffering` is necessary for us to be able
to display the prompt. We apparently don't need to understand why that's true quite yet.

13.8 `do` syntax and IO
-----

`do` lets us sequence _monadic actions_ and unpack the results of those monadic actions
into named things we can use later.

We can, in the repl for example, do:

```haskell
Prelude> let x = getLine
Prelude> let y = getLine
Prelude> let z = getLine
Prelude> do {foo <- x; bar <- y; baz <- z; putStrLn $ foo ++ bar ++ baz}
```

And there'd be more examples if we knew other monadic actions like `IO a`

### return

`return`... lifts (? is that the right word?) a value of type `a` to a `Monad a`.

So this works:

```haskell
Prelude> let x = getLine
Prelude> let y = return "foo"
Prelude> let z = getLine
Prelude> do {foo <- x; bar <- y; baz <- z; putStrLn $ foo ++ bar ++ baz}
```

But this doesn't:

```haskell
Prelude> let x = getLine
Prelude> let y = "foo"
Prelude> let z = getLine
Prelude> do {foo <- x; bar <- y; baz <- z; putStrLn $ foo ++ bar ++ baz}
```

Sometimes `do` blocks will end `return ()` to indicate that everything's done
and to let you know that you don't get anything back from the `do` block

13.9 Hangman game
-----

Seriously, what the hell

```
Guess a letter: Your guess must be a single character
Current puzzle is: e l _ _ i a n Guessed so far: fpdmhgr

You lose!
The word was: elysian
```

Other fun words my hangman has picked: `maurice`, `dagger's`, `petiole`
