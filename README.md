# Gestalt [![Build Status](https://travis-ci.org/liufengyun/gestalt.svg?branch=master)](https://travis-ci.org/liufengyun/gestalt)

An experiment and tutorial on how to create a solid and portable macros system based on solid [methodology and principles](https://www.dropbox.com/s/2xzcczr3q77veg1/gestalt.pdf).

Design Goals:

- __Portable__ -- better IDE experience, works both in Scala and Dotty
- __Solid__ --  friendly typing error instead of compiler crashes or hacks
- __Friendly__ -- no knowledge about compiler internals, no dependent universes

## Examples

```Scala
import scala.gestalt._

object Test {
  def plus(a: Int, b: Int): Int = meta {
    q"$a + $b"
  }
}
```

## Design

- Only blackbox macros
- Separation of _typed_ and _untyped_ trees
- Only inspection of _typed trees_
- Only semantic information for _typed trees_

## Philosophy

The design space of a macro system is large. **Gestalt aims to be simple, solid and portable for the 80% common use cases**.

a ) Assumptions on compiler implementation
-  more assumptions
-  fewer assumptions (✓) : portability & easy implementation

b ) Assumptions on macro usage
- more assumptions (✓) : focus on 80% common cases, make them easy
- fewer assumptions

c) Features
-  more features
-  fewer features (✓) : metaprogramming is hard, restrict power, implementation solid


## Test

- `sbt macros/test:run`

## Paper

[Gestalt : The Road for Lightweight Portable Macros](https://www.dropbox.com/s/2xzcczr3q77veg1/gestalt.pdf)

