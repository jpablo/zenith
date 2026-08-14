# Zenith formalization

This folder contains optional machine-checked statements about Zenith.

* `CoreLaws.lean` proves pure laws for causes, exits, interruption status, and
  ordinary environment projection.
* `TypeAlgebra.lean` formalizes the abstract requirement-intersection and
  error-union algebra.
* `ServiceKeyLaws.lean` formalizes service-key rows and their laws. It supports
  the optional `Zenith.Services` layer API and documents the properties that
  the runtime code must preserve.

This is a separate optional library. Application programs do not need to
import it.
