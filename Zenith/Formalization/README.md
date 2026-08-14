# Keyed-service formalization

This folder contains machine-checked statements about the keyed-service model.

`ServiceKeyLaws.lean` formalizes service-key rows and their laws. It supports
the optional `Zenith.Services` layer API and documents the properties that the
runtime code must preserve.

This is a separate optional library. Application programs do not need to
import it.
