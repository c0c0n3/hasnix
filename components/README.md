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





[cabal.dia]: ./build-graph.png
[concept.dia]: ./components.png
