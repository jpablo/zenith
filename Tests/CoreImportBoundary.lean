import Z

/-!
Regression guard for the core-library import boundary.

HTTP, debugging, and keyed services are optional integrations. A program that
imports only `Z` must not gain their Zenith APIs.

This module is not part of the runtime test suite. The check is that it
compiles while the following command fails to elaborate.
-/

#check_failure Zenith.Http.App
#check_failure Zenith.Debug.runWithGraphviz
#check_failure KeyedLayer
