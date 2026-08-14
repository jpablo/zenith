import Tests.Support

open Std

namespace HttpTests

private def loopback : Net.SocketAddress :=
  .v4 ⟨.ofParts 127 0 0 1, 0⟩

private def request : IO (Std.Http.Request Std.Http.Body.Stream) := do
  let body ← (Std.Http.Body.empty).block
  pure <| Std.Http.Request.new.body body

def testAppAdapter : IO Unit := do
  let successful : Z.Http.App IO.Error := {
    handle := fun _ => Z.Http.Response.text "ok"
  }
  let successfulResponse ←
    (successful.toHandler.onRequest (← request)).run.block
  assertTrue "a successful HTTP app did not return 200"
    (successfulResponse.line.status.toCode == 200)

  let providedHandler : Z.Http.Request -> Z String IO.Error Z.Http.Response :=
    fun _ => Z.serviceWithM Z.Http.Response.text
  let provided := Z.Http.App.provide providedHandler "provided"
  let providedResponse ←
    (provided.toHandler.onRequest (← request)).run.block
  assertTrue "App.provide did not close the app environment"
    (providedResponse.line.status.toCode == 200)

  let failing : Z.Http.App String := {
    handle := fun _ => (Z.fail "handler failure").map impossible
  }
  let failureResponse ←
    (failing.toHandler.onRequest (← request)).run.block
  assertTrue "a typed HTTP app failure did not return 500"
    (failureResponse.line.status.toCode == 500)

def testScopedServerLifecycle : IO Unit := do
  let app : Z.Http.App IO.Error := {
    handle := fun _ => Z.Http.Response.text "lifecycle"
  }
  let program : Z Unit IO.Error Unit := Z.scoped do
    let server ← Z.Http.Server.acquire loopback app
    if server.localAddress?.isSome then
      Z.succeed ()
    else
      Z.fail (IO.userError "the HTTP server did not report its bound address")
  match ← runProgram "http-server-lifecycle" program with
  | .success () => pure ()
  | _ => failTest "the scoped HTTP server did not start and stop cleanly"

def httpTests : List (String × IO Unit) := [
  ("testAppAdapter", testAppAdapter),
  ("testScopedServerLifecycle", testScopedServerLifecycle)
]

end HttpTests
