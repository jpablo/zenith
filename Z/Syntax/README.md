# Zenith syntax

This folder contains syntax elaborators for Zenith programs.

`Do.lean` defines `zdo`, which is Zenith's typed `do` notation. It infers the
combined environment and typed error channel across the statements in a block.

The syntax is available through `import Z`.
