import Z

/-!
Regression guard for the core-library import boundary.

HTTP is an optional integration. A program that imports only `Z` must not gain
the `Zenith.Http` API or its standard HTTP dependency.

This module is not part of the runtime test suite. The check is that it
compiles while the following command fails to elaborate.
-/

#check_failure Zenith.Http.App
