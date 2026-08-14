# HTTP server integration

`Zenith.Http` is an optional integration with Lean's standard
`Std.Http.Server`. It is not part of the core `Z` library: applications that
need it import `Zenith.Http` explicitly. Zenith
does not parse HTTP, own sockets, or reimplement HTTP/1.1. The standard library
does that work. Zenith owns the application effect and the server lifetime.

## Main types

- `Zenith.Http.App E` is a closed request handler:
  `Zenith.Http.Request -> Z Unit E Zenith.Http.Response`.
- `Zenith.Http.Response.text` and `Zenith.Http.Response.notFound` create UTF-8
  responses.
- `Zenith.Http.Server.start` binds and starts an app.
- `Zenith.Http.Server.acquire` starts a server in `Scope` and stops it when the scope
  closes.
- `Zenith.Http.Server.serve` starts a scoped server and waits for its shutdown.

An app can close an environment-dependent handler with
`Zenith.Http.App.provide`.
This keeps the standard callback interface closed while application setup still
uses Zenith layers and services.

## Failure rule

Lean's standard HTTP callback has no typed error channel. Therefore, if an app
fails with a typed failure, a defect, or interruption, Zenith returns an HTTP
500 response with the fixed body `Internal Server Error`.

## Runnable demo

Run:

```sh
lake exe httpServer
```

In another terminal:

```sh
curl http://127.0.0.1:8080/health
```

The demo returns `ok`. Press Enter in the server terminal to close its scope.
The scope stops the server and waits for active standard HTTP connections to
close.

## Current boundary

Each standard HTTP callback runs the closed Zenith app with `Z.unsafeRunSync`.
This is a useful first integration: it preserves Zenith typed failures and
resource lifetime. It does not yet connect standard HTTP request cancellation to
Zenith fiber interruption. Streaming request and response APIs can add that in a
later step.
