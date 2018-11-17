# What this is

These are notes and transcribed functions from my reading of The Reasoned Schemer, which minikanren.org calls "The Book". (Mini-kanren is a language resembling Prolog, but which only takes about 300 lines of code to implement.)

# An easy way to use it

You'll need Racket installed, and need to clone this repo.

As described [here](https://www.monolune.com/using-racket-for-the-reasoned-schemer/), you can install the canonical mini-kanren implementation for Racket by running `raco pkg install github://github.com/miniKanren/Racket-miniKanren/master` from the command line.

Once that's done, if you open Racket from the root of this repository, the code in this repository can be loaded by running `(load "load.rkt")`.
