import Std.Http
import Z.Resource.Scope

/-!
Thin integration with Lean's `Std.Http.Server`.

`Zenith.Http` is an optional integration. The core `Z` library does not import
it. Zenith owns the application effect and the server lifetime. Lean's standard
library owns HTTP parsing, sockets, request bodies, and response streaming.
Typed application failures and defects become an HTTP 500 response because the
standard server callback has no typed error channel.
-/

namespace Zenith.Http

open Std

/-- An incoming standard HTTP request with a streaming body. -/
abbrev Request := Std.Http.Request Std.Http.Body.Stream

/-- A standard HTTP response with a type-erased streaming body. -/
abbrev Response := Std.Http.Response Std.Http.Body.Any

namespace Response

/-- Build a UTF-8 text response with the specified standard HTTP status. -/
def textWith
    (status : Std.Http.Status)
    (content : String) : Z Unit IO.Error Response :=
  Z.attempt do
    let response ← (Std.Http.Response.withStatus status).text content |>.block
    pure response

/-- Build a successful UTF-8 text response. -/
def text (content : String) : Z Unit IO.Error Response :=
  textWith .ok content

/-- Build a 404 UTF-8 text response. -/
def notFound (content : String) : Z Unit IO.Error Response :=
  textWith .notFound content

end Response

/-- A closed Zenith HTTP application. -/
structure App (E : Type) where
  handle : Request -> Z Unit E Response

namespace App

/-- Close an environment-dependent request handler into a standard HTTP app. -/
def provide
    (handler : Request -> Z R E Response)
    (environment : Environment R) : App E :=
  ⟨fun request => (handler request).provideEnvironment environment⟩

private def failureResponse : Std.Async.ContextAsync Response :=
  Std.Http.Response.internalServerError.text "Internal Server Error"

/-- Adapt an app to Lean's standard HTTP-server callback interface. -/
def toHandler (self : App E) : Std.Http.Server.StatelessHandler :=
  Std.Http.Server.Handler.ofFn fun request => do
    let exit ← Z.unsafeRunSync (self.handle request) "http-request"
    match exit with
    | .success response => pure response
    | .failure _ => failureResponse

end App

/-- A running standard HTTP server owned by a Zenith scope. -/
structure Server where
  private raw : Std.Http.Server

namespace Server

/-- Return the address assigned to a socket-backed server. -/
def localAddress? (self : Server) : Option Net.SocketAddress :=
  self.raw.localAddr

/-- Bind an app to `address` and start accepting standard HTTP connections. -/
def start
    (address : Net.SocketAddress)
    (app : App E)
    (config : Std.Http.Config := {}) : Z Unit IO.Error Server :=
  Z.attempt do
    let raw ← (Std.Http.Server.serve address app.toHandler config).block
    pure ⟨raw⟩

/-- Stop the server and wait for its active standard HTTP connections to close. -/
def shutdown (self : Server) : Z Unit Empty Unit :=
  Z.fromIO <| self.raw.shutdownAndWait.block

/-- Wait until another operation stops the server. -/
def awaitShutdown (self : Server) : Z Unit IO.Error Unit :=
  Z.attempt <| self.raw.waitShutdown.block

/-- Acquire a server in the current `Scope` and stop it when that scope closes. -/
def acquire
    (address : Net.SocketAddress)
    (app : App E)
    (config : Std.Http.Config := {}) : Z Scope IO.Error Server :=
  Z.acquireRelease (start address app config) fun server => server.shutdown

/-- Run a server until it is stopped or the calling fiber is interrupted. -/
def serve
    (address : Net.SocketAddress)
    (app : App E)
    (config : Std.Http.Config := {}) : Z Unit IO.Error Unit :=
  Z.scoped do
    let server ← acquire address app config
    server.awaitShutdown

end Server
end Zenith.Http
