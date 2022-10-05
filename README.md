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

## Building and running examples

* [Elan](https://github.com/leanprover/elan) is needed to install `lake`, which will in turn download project dependencies (i.e. the specific Lean 4 version).
* `VSCode` + `Lean 4` plugin is the recommended editor.

#### Compile everything once

```bash
lake build
```

#### Recompile on chage

Using the [`entr`](https://github.com/clibs/entr) file monitor

```bash
find . -name "*.lean" | entr -s 'lake build'
```

#### Run example programs

```bash
./build/bin/z
```

#### Regenerate svg diagrams

(Graphviz needs to be installed)

```bash
for f in $(find diagrams -name "*.dot"); do echo $f; dot -Tsvg $f -o diagrams/$(basename $f .dot).svg; done
```

This will (re)create a bunch of svg files under `diagrams/*`.

## Internal documentation

* [Interpreter](docs/run-loop.md)
* [Module dependencies](docs/module-dependencies.md)
* [Variance](docs/variance.md)
* [Problems](docs/Problems.md)