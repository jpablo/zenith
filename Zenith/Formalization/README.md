# Zenith formalization

This folder contains optional machine-checked statements about Zenith.

* `CoreLaws.lean` proves pure laws for causes, exits, interruption status, and
  ordinary environment projection.
* `TypeAlgebra.lean` formalizes the abstract requirement-intersection and
  error-union algebra, including generic canonical normal forms.
* `ServiceKeyLaws.lean` formalizes service-key rows and their laws. It supports
  the optional `Zenith.Services` layer API and documents the properties that
  the runtime code must preserve.
* `ServiceRowConnection.lean` connects stable keyed rows and typed projections
  to the abstract requirement algebra.
* `ErrorShape.lean` models the public nested-`Sum` error representation and
  maps it to the abstract error algebra.
* `VarianceLaws.lean` checks the production `Z` variance, coercion, and
  heterogeneous composition signatures.

This is a separate optional library. Application programs do not need to
import it.
