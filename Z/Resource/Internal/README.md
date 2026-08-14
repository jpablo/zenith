# Internal resource runtime

This folder contains implementation support for `Z.Resource`.

`HEIO.lean` defines the high-universe effect used to build and release layers.
It permits layer inputs and outputs above Lean's normal `Type` universe while
keeping `Z` effects in their public representation.

Application code should use `Layer` and `Z` APIs. It should not depend on this
module directly.
