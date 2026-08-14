import Z
import Zenith.Http

open Std

/-!
A small runnable Zenith HTTP server.

Run `lake exe httpServer`, then request `http://127.0.0.1:8080/health` from
another terminal. Press Enter in the server terminal to stop it. `Z.scoped`
owns the listening socket and waits for the standard server to shut down.
-/

private def address : Net.SocketAddress :=
  .v4 ⟨.ofParts 127 0 0 1, 8080⟩

private def app : Zenith.Http.App IO.Error where
  handle request :=
    if toString request.line.uri == "/health" then
      Zenith.Http.Response.text "ok\n"
    else
      Zenith.Http.Response.notFound "Try GET /health\n"

private def program : Z Console IO.Error Unit :=
  Z.scoped <| zdo
    let _server ← Zenith.Http.Server.acquire address app
    Console.printLineM "Listening on http://127.0.0.1:8080/health"
    Console.printLineM "Press Enter to stop the server."
    let _ ← Console.readLineM
    Console.printLineM "Stopping server."

def main : IO Unit := do
  match ← Z.unsafeRunSync
      (program.provideEnvironment Console.live) "http-server-demo" with
  | .success () => pure ()
  | .failure cause =>
      throw (IO.userError s!"HTTP server demo failed: {cause}")
