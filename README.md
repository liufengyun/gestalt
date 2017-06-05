# Gestalt [![Build Status](https://travis-ci.org/liufengyun/gestalt.svg?branch=master)](https://travis-ci.org/liufengyun/gestalt)

An experiment and tutorial on how to create a solid and portable macros system based on solid [methodology and principles](https://www.dropbox.com/s/2xzcczr3q77veg1/gestalt.pdf).

Design Goals:

- __Portable__ -- better IDE experience, works both in Scala and Dotty
- __Solid__ -- compiler crashes to a minimum, no hack required in writing macros
- __Friendly__ -- no knowledge about compiler internals, no dependent universes
- __Hygienic__ -- hygiene guaranteed by the type system

## Examples

```Scala
import scala.gestalt.api_

object Test {
  def plus(a: Int, b: Int): Int = meta {
    q"$a + $b"
  }
}
```

## Features

Gestalt advances the state of the art of meta-programming in Scala:

- Macros are portable among different Scala compilers
- Better IDE support due to portability
- Hygienic macros ensured by type system
- Automatic owner chain management
- Type system defends against incorrect mixing of typed and untyped trees
- Friendly semantic APIs
- No inconvenience related to path-dependent universes
- Easy to implement by compiler vendors

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

