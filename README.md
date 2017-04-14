# Gestalt [![Build Status](https://travis-ci.org/liufengyun/gestalt.svg?branch=master)](https://travis-ci.org/liufengyun/gestalt)

portable and lightweight macros with semantic API

Status: active development, not 0.1 yet.

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

[Gestalt : The Road for Lightweight Portable Macros](https://dl.dropboxusercontent.com/u/54580502/gestalt.pdf)

