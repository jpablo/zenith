# Zenith

`Zenith` is a Lean 4 library inspired by [ZIO](https://zio.dev/).

The main goals are pedagogical:

* Explore how to translate certain OO patterns into Lean
* Simple implementation of ZIO better suited for high level analysis
* Visualization of program execution
* etc

## Features

* Dependency Injection
* Error handling
* Asynchronous and Concurrent programming
* Program execution diagram

## Status

* Core data type and interpreter ✅
* Fibers ✅
* Environment ✅
* Layers and automatic keyed composition ✅
* Constructor-derived layers ✅
* Dynamic resource scopes ✅

Pure service constructors can become layers without manual environment
projection:

```lean
structure Repository where
  database : Database
  config : AppConfig

def makeRepository (database : Database) (config : AppConfig) : Repository :=
  { database, config }

def repositoryLayer :=
  KeyedLayer.derive makeRepository
```

The result requires `Database` and `AppConfig` and provides `Repository`.
`KeyedLayer.derive[Service]` is a shorthand that uses a structure constructor.
Use `Layer.fromZ` or `Layer.acquireReleaseZ` when construction is effectful or
owns a resource.

## Building and running examples

* [Elan](https://github.com/leanprover/elan) is needed to install `lake`, which will in turn download project dependencies (i.e. the specific Lean 4 version).
* `VSCode` + `Lean 4` plugin is the recommended editor.

#### Compile everything once

```bash
lake build
```

#### Run all regression checks

```bash
lake test
```

#### Recompile on change

Using the [`entr`](https://github.com/clibs/entr) file monitor

```bash
find . -name "*.lean" | entr -s 'lake build'
```

#### Run example programs

```bash
lake exe z
```

Run the standalone GitHub issue-sync demo:

```bash
lake exe githubIssueSync
```

Run the dynamic resource scope demo:

```bash
lake exe scopedResource
```

Generate a Markdown TODO report for a workspace:

```bash
lake exe todoReport [ROOT] [OUTPUT]
```

`ROOT` defaults to the current directory. `OUTPUT` defaults to
`ROOT/todo-report.md`. A relative `OUTPUT` is resolved from `ROOT`.

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
* [Dynamic resource scopes](docs/scopes.md)
