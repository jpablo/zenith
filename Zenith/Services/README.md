# Keyed services and automatic layers

This folder implements optional service-key support for Zenith applications.

* `Model.lean` defines service keys and keyed-layer data.
* `Environment.lean` defines the keyed service environment.
* `Layer.lean` composes keyed layers and provides services to a program.
* `Derive.lean` derives a layer from a constructor function.
* `Syntax.lean` defines the service and layer syntax.
* `../Services.lean` is the public import module.

Import `Zenith.Services` to use `Services[...]`, `KeyedLayer`,
`KeyedLayer.derive`, or `Z.provide`. Their public declarations extend the `Z`
vocabulary, but this package remains optional.
