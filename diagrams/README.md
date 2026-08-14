# Execution diagrams

This folder contains execution traces for the examples.

Each `.dot` file is Graphviz source. The matching `.svg` file is its rendered
diagram. The diagrams show interpreter instructions, continuations, fibers,
and final exits.

Generate new DOT output with `Zenith.Debug.runWithGraphviz`. Regenerate SVG
files with Graphviz:

```sh
for file in diagrams/*.dot; do
  dot -Tsvg "$file" -o "${file%.dot}.svg"
done
```
