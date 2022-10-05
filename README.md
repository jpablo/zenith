# Zenith

`Zenith` is a Lean 4 library inspired by [ZIO](https://zio.dev/).

Right now the main goals are pedagogical:

* Explore how to translate certain OO patterns into Lean
* Simple implementation of ZIO better suited for high level analysis
* Visualization of program execution
* etc

But once the library matures enough it should be usable for regular programming.

## Features

* Dependency Injection
* Error handling
* Asynchronous and Concurrent programming
* Program execution diagram

## Status

* Core data type and interpreter ✅
* Fibers ✅
* Environment ✅
* Layers ❌

## Future plans

* Find a workaround for the limitation that `Z` Services can't be used in environment.
* Scopes and FiberRefs
* Fiber supervision
* Concurrency primitives
* Better integration with Lean features