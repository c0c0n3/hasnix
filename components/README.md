The expression headaches
------------------------
> Enough tagless final interpreters to keep Cabal busy.

So I need some code to whip together Cabal files with examples of
building libs, exes, tests, benchmarks and whatnot. Have to split
up the code in more than one package to pretend this is a real-life
program made up by a bunch of components each in its own Cabal package.

Since I'm in for self-punishment, why not code a solution to the
"Expression" problem? Luckily my famed C&P tech never failed me and
in fact I'm going to shamelessly lift a great solution from these
excellent lecture notes:

* http://okmij.org/ftp/tagless-final/course/lecture.pdf

The oke who wrote that really has a knack for making complex stuff
understandable but my limited brain cell supply requires I should keep
headache tablets at hand as try to understand what I can grab from
those notes.


### Conceptual model

Many headaches later, I've finally managed to pull together the components
for my toy program. It's a silly REST service that lets you add and
multiply integers. Two components lay the arithmetic bricks in tagless
final style:

* `expr`. Defines the `ExprF` typeclass to deal with abstract "addition"
   of representations. (The "F" there is to remind me I'd like to think
   of this as a clever encoding of an initial F-algebra. If my memory
   serves me well, someone actually made that connection explicit.)
   This component comes with two tagelss final interpreters: a calculator
   to add integers and a pretty-printer to show S-expressions.
* `expr extension`. Extends `ExprF` with "multiplication" of representations
   through the `MultF` typeclass. It also extends the two "addition"
   tagelss final interpreters to cater to "multiplication".

The arithmetic language gets defined by stringing together `ExprF`
and `MultF`. (I like to think of this as a co-product in disguise!)
To be able to claim victory over the Expression problem, each component
lives in its own Haskell package---see below. Another component, `servo`,
implements a Servant REST service that lets you POST a JSON doc encoding
an arithmetic expression and then redirects you to a resource representing
the result of evaluating it. The resource's URL contains both the computed
result and the `sexpr` corresponding to the input JSON. To do that,
`servo` has to solve the "Deserialisation" problem, a nasty variant
of the "Expression" problem, to be able to deserialise the input JSON
to a language expression.
For variety, there's also a console app, `expro`, that evaluates and
pretty-prints a hard-coded language expression. Here's how I picture
the whole thing in my head:

![Conceptual model.][concept.dia]


### Haskell packages

So we've got libs, exes, tests, benchmarks, and even a Web app to
make sure we'll have enough fun with Cabal. There's four Haskell
(Cabal) packages:

* `expr`. A lib implementing the `expr` component. Plus there's Tasty/Hedgehog
  prop-based tests with tricky recursive generators for our language
  expressions. Notice how some of the lib functionality is available
  to test code but not to other packages.
* `expr-extension`. Implements (surprise, surprise!) the `expr extension` 
  component and the `expro` console app. It comes with a Criterion
  benchmark made up by two groups—one to evaluate plain expressions
  the other for tagelss expressions. (I love deeply pointless examples,
  see `ExprBuilder` to understand why.) There's also an example of how
  to turn off some warnings for a specific file (while keeping them on
  for all the others) using the `OPTIONS_GHC` pragma.
* `expr-server`. The REST service, `servo`, done the Servant way. Besides
   all the type-safe goodness Servant brings to the party, there's also
   Aeson custom deriving of recursive types and a Cabal-auto-generated
   module to access the version number declared in the Cabal file.
* `util`. Convenience to re-export goodies I use across the board
   within the project, like Unicode symbols.

Most code gets compiled with strict type safety on to force myself
to write Safe Haskell whenever I can. The GHC manual says it's good
practice, but it doesn't look like most of the Haskell community actually
give a toss? So perhaps it isn't a practical thing to do since trying
to use libs not compiled with Safe Haskell from Safe Haskell code is
a royal pain in...yah, there. (Chances are I'm doing something wrong
though!) Also, I'm using Unicode all over the show which I don't think
is a community-blessed practice, but I couldn't resist the cuteness.


### Cabal project

So I've got enough stuff to play with Cabal 3.0 and most of its new
features—well, new to me, I haven't used Cabal in ages. Admittedly,
I went overboard with the Haskell code, but it was so much fun I
just couldn't stop myself. Anyhoo, there's a Cabal project file next
to this README that adds all the Haskell packages in this dir to the
Cabal project so any Cabal package file in the sub-dirs gets sucked
in. Oh dear, perhaps it isn't such a bad thing but most Cabal project
files I looked at on the interwebs are explicit about what to include.

Here's a little build graph of sorts showing what each Cabal file
builds and the build dependencies. Build targets have a dartboard
icon with a tag to tell its type—library, test executable, and so
on. Also, a `+ release` tag means the build target gets tweaked with
prod settings when you pass the `release` flag to Cabal. The head
of a dashed arrow points to the build target or set of external libs
the tail depends on.

![Build graph.][cabal.dia]

I haven't been stingy with comments in those Cabal files. No, I'm
not tryna delight the hapless internaut who might venture in there
with my grematically-chalinged  writing. I happen to have the memory
of a gold fish and sure as hell tomorrow I won't remember what I've
learnt about Cabal today, well unless I jot it down. But here's the
highlights of those Cabal files:

* `common` stanzas to stop me using my famed C&P tech. For example,
  sources get shared among targets within a Cabal file as do common
  GHC options & warnings, external libs dependencies, and so on.
  I pushed the envelope and tried to put bits and pieces common to
  all package files into the project file and then share them from
  there, but I failed miserably—actually, I'm not even sure that kind
  of thing is even supported?
* Package private lib modules that other packages can't import but
  that other modules in a separate compilation unit (exe, test, benchmark)
  within the same package can still use.
* Cabal autogen modules.
* Default extensions for all the modules in a package.
* Warnings about every dodgy code block under the sun. (`-Weverything`
  minus a few annoying warnings.)
* Parallel RTS options.
* Release flags to optimise release builds and make GHC warnings fail
  the build.
* Hackage state pin for reproducible builds, well sort of, read below.

Oh I should also mention that except for the warning about `-O2`,
`cabal check` is happy about each and every Cabal file in the project.


### Cabal's Nix-style builds

Cabal took inspiration from Nix to get us out of dependency hell and
give us reproducible builds. (Read up about Nix-style local builds.)
If you pin the Hackage state in your Cabal project file, then builds
should be reproducible across boxes. (Also I'm not sure if you still
need Cabal `freeze` with a pinned state, why would you?) Now I hate
to be a reproducibility party pooper, but I think that's true as
long as you don't have a (either direct or indirect) dependency on
a native lib.

The way I understand it, given the set of packages (and their dependency
descriptions) `H(t)` in Hackage at time `t` and a build target `b`
in your Cabal file, the solver figures out the set `𝒮` of packages
needed to build `b`. Depending on version bounds, I reckon this set
could be empty, i.e. there's no way you can possibly build `b`, but
I'm digressing. I'm going to think of this process of picking packages
to build `b` as a function

    𝒮(b, H(t)) = { mypkg-1.3.0.5, upkg-2.1.0.0, ... }

Surely it can happen that for `t < t'`, `H(t) ≠ H(t')` and then also
(but not necessarily) `𝒮(b, H(t)) ≠ 𝒮(b, H(t'))`. Now pinning the index
state means we stick to some `H(t)` so you get the same build plan
regardless of which cyber planet you live on. But, I don't think Cabal
can take care of installing native dependencies for you, possibly in
a sand-boxed environment the way Nix does. So if `b` depends, perhaps
indirectly, on a native lib, it could potentially break in a galaxy
far, far away.

In fact, I hit a snag. Surely I could be dead wrong and likely there's
something I'm still missing about Nix-style builds, but with an index
state of `2021-02-07T15:40:32Z` on a box without `zlib` (C lib) I get

    $ cabal build all
    Failed to build zlib-0.6.2.2 ...
    ...
    cabal: Missing dependency on a foreign library:
    * Missing (or bad) header file: zlib.h
    * Missing (or bad) C library: z
    ...

Now even if I installed `zlib` myself, the version I install could
be different than the one on the other box where I managed to build
without a glitch. At the end of the day, it looks like you'll need
something like Nix (or Guix) to reproduce builds reliably.


### Cabal + Nix

Why not run Cabal builds in a Nix shell with Cabal, GHC and all C
libs your Haskell packages depend on? In fact, even if I had no deps
on native libs, I'd still have to make sure GHC and Cabal are the
same on all build boxes, so Nix comes in handy. Now Cabal & Nix sounds
like a match made in heaven and Cabal 3 even supports Nix integration
so you could take it to the next level, having Cabal pick Haskell
packages from your Nix store instead of Hackage. Happiness? Mostly,
but there's a couple of things to keep in mind.

Like before, I'm going to think of this process of picking packages
as a function `𝒮'(b, N(t))` where `N` is the set of Haskell packages
in Nixpkgs on GitHub at time point `t`. If my memory serves me well,
`N(t)` gets built from `H(ηt)` for some `ηt ≤ t`, includes at least
one version of each package in `H(ηt)` but not all versions of every
package. Surely then

    ∃ t . N(t) ≠ H(ηt)

Actually assuming packages/versions never get deleted from Hackage,
we should have `∀ t . N(t) ⊂ H(ηt) ⊂ H(t)`, but regardless that means
it's definitely possible that

    ∃ t . 𝒮(b, H(ηt)) ≠ 𝒮'(b, N(t))        (1)

i.e. the set of packages the solver selects to build `b` when using
vanilla Cabal with an index state of `ηt` isn't necessarily the same
as that the solver spits out when running Cabal in a Nix shell with
the Haskell Nix packages at `t`. In particular it could even happen
that you can build with vanilla Cabal, but then under Nix there's
no combination of packages/versions in `N(t)` that satisfy all the
build constraints, so you can't actually build `b`

    ∃ t . 𝒮(b, H(ηt)) ≠ ∅  ∧ 𝒮'(b, N(t)) = ∅        (2)

Here's a diagram to sum up all I've said up until now about Hackage,
Nix and Cabal build plans.

![Cabal build plans with Hackage and Nixpkgs.][build-plans.dia]

To see how the sets `𝒮` and `𝒮'` pick could wind up being slightly
different, install `nix` and pin Nixpkgs at [2080afd0][nix-pin]. Then

    $ nix-env -i ghc cabal
    $ cabal update  # index state at 2021-02-07T15:40:32Z, see cabal.project

run `cabal build` on all targets, and take note of the package versions
that got pulled down from Hackage—you could use [cabal-plan][cabal-plan]
for that. Now zap `~/.cabal`, `cabal clean` and get a Nix shell with
all project deps

    $ nix-shell -p "haskellPackages.ghcWithPackages (p: \
         [p.aeson p.base-unicode-symbols p.criterion p.hedgehog p.tasty \
          p.tasty-hedgehog p.servant-server p.wai p.warp])"

After rebuilding all targets, eyeball the packages you've got this
time around. The sets that `𝒮` and `𝒮'` picked should be almost the
same, except earlier `𝒮` should've selected `aeson-1.5.5.1`, `vector-0.12.2.0`,
`wai-3.2.3`, `warp-3.3.14`, `hedgehog-1.0.4`, and `tasty-hedgehog-1.0.1.0`
whereas now `𝒮'` should've downgraded these packages to slightly older
versions.

Do I give a hoot about all this? Well, yes if I want reproducible builds.
For that to happen, the package set the Cabal solver picks has to be
the same in every build env, so, because of (1), I can either source
packages from Hackage or Nix. For example, I should avoid building
locally with vanilla Cabal and then have a CI set up where Cabal runs
with Nix integration. Ideally, I should pick one of these options and
stick with it:

* A Nix shell with GHC, Cabal and any required native libs. Cabal
  builds inside that shell but uses a pinned Hackage index state.
* A Nix shell like the above + all Haskell packages required to
  build. Cabal builds inside that shell using the Nix package set.
  (Nixpkgs has to get pinned to a specific Git revision obviously.)
* [haskell.nix][haskell.nix]. A breadth of fresh air, trying to get
  the best of both worlds, but it's still in its infancy.

The second option could be a pain in the backside if for all of your
choices of `t`, `𝒮'(b, N(t)) = ∅ `. In that case, you'll have to add
packages to `N(t)`, e.g. using Nix overlays, until you get to the
point where `𝒮'(b, N(t)) ≠ ∅`. With luck, that could mean adding just
a couple of packages, but you could be in for a world of pain if you
have to add dozens instead—think indirect dependencies. This is where
[haskell.nix][haskell.nix] could help: it actually generates the Nix
Haskell package set for you, starting from the Cabal build plan
(`plan.json`), i.e. `𝒮(b, H(t))` where `t` is the Hackage index state
specified in the Cabal project file. Well at least, that's what I
think it does, but as usual I could be wrong.




[build-plans.dia]: ./build-plans.png
[cabal.dia]: ./build-graph.png
[cabal-plan]: https://hackage.haskell.org/package/cabal-plan
[concept.dia]: ./components.png
[haskell.nix]: https://github.com/input-output-hk/haskell.nix
[nix-pin]: https://api.github.com/repos/NixOS/nixpkgs/commits/2080afd0
