import Z

/-!
A larger `zdo` example based on a GitHub issue synchronization job.

The example uses four services and three source error types. `syncRaw` shows
the inferred environment and error union. `sync` catches source and audit
errors, and it always runs an audit finalizer.
-/

namespace GithubIssueSync

structure Issue where
  id : Nat
  title : String
  deriving BEq, Repr

structure SyncConfig where
  organization : String
  dryRun : Bool
  deriving BEq, Repr

inductive ConfigError where
  | unavailable
  deriving BEq, Repr

inductive GithubError where
  | unavailable
  deriving BEq, Repr

inductive StoreError where
  | writeFailed (issueId : Nat)
  deriving BEq, Repr

inductive AuditError where
  | unavailable
  deriving BEq, Repr

instance : ToString ConfigError where
  toString
    | .unavailable => "configuration unavailable"

instance : ToString GithubError where
  toString
    | .unavailable => "GitHub unavailable"

instance : ToString StoreError where
  toString
    | .writeFailed issueId => s!"cannot save issue {issueId}"

instance : ToString AuditError where
  toString
    | .unavailable => "audit unavailable"

structure ConfigService : Type 1 where
  load : Z Unit ConfigError SyncConfig

structure GithubService : Type 1 where
  openIssues : String -> Z Unit GithubError (List Issue)

structure IssueStore : Type 1 where
  save : Issue -> Z Unit StoreError Unit

structure Audit : Type 1 where
  recordFailure : String -> Z Unit AuditError Unit
  finish : Z Unit Empty Unit

abbrev SourceErrors := ConfigError ⊕ GithubError ⊕ StoreError
abbrev AllErrors := AuditError ⊕ ConfigError ⊕ GithubError ⊕ StoreError
abbrev RawServices := ConfigService × GithubService × IssueStore
abbrev Services := Audit × ConfigService × GithubService × IssueStore

def loadConfig : Z ConfigService ConfigError SyncConfig :=
  Z.serviceWithZ fun service => service.load

def getOpenIssues
    (organization : String) : Z GithubService GithubError (List Issue) :=
  Z.serviceWithZ fun service => service.openIssues organization

def saveIssue (issue : Issue) : Z IssueStore StoreError Unit :=
  Z.serviceWithZ fun service => service.save issue

def recordFailure (message : String) : Z Audit AuditError Unit :=
  Z.serviceWithZ fun service => service.recordFailure message

def finishAudit : Z Audit Empty Unit :=
  Z.serviceWithZ fun service => service.finish

def describeSourceError : SourceErrors -> String
  | .inl .unavailable => "configuration unavailable"
  | .inr (.inl .unavailable) => "GitHub unavailable"
  | .inr (.inr (.writeFailed issueId)) =>
      s!"cannot save issue {issueId}"

/-- Synchronize all open issues without error recovery. -/
def syncRaw := zdo
  let config <- loadConfig
  let issues <- getOpenIssues config.organization
  if config.dryRun then
    pure issues.length
  else
    for issue in issues do
      saveIssue issue
    pure issues.length

example : Z RawServices SourceErrors Nat := syncRaw

/-!
These two programs request the same services and errors in opposite orders.
Their checked types show that inference does not depend on action order.
-/

def requirementsForward := zdo
  let config <- loadConfig
  let _ <- getOpenIssues config.organization
  let _ <- saveIssue { id := 1, title := "forward" }
  recordFailure "forward"

def requirementsReverse := zdo
  let _ <- recordFailure "reverse"
  let _ <- saveIssue { id := 1, title := "reverse" }
  let _ <- getOpenIssues "lean"
  let _ <- loadConfig
  pure ()

example : Z Services AllErrors Unit := requirementsForward
example : Z Services AllErrors Unit := requirementsReverse

/-!
The first handler converts a source failure into an audit action. If that
action fails, the second handler catches `AuditError`. The finalizer runs after
all success and failure paths.
-/

def sync := zdo
  try
    syncRaw
  catch sourceError =>
    let _ <- recordFailure (describeSourceError sourceError)
    pure 0
  catch _ =>
    pure 0
  finally
    finishAudit

example : Z Services Empty Nat := sync

/-- Build the three services that `syncRaw` needs. -/
def rawLayer
    (config : ConfigService)
    (github : GithubService)
    (store : IssueStore) : Layer Unit Empty RawServices :=
  let configLayer : Layer Unit Empty ConfigService := Layer.succeed config
  let githubLayer : Layer Unit Empty GithubService := Layer.succeed github
  let storeLayer : Layer Unit Empty IssueStore := Layer.succeed store
  configLayer.zipWith
    (githubLayer.zipWith storeLayer fun github store => (github, store))
    fun config services => (config, services)

/-- Build all four services in the normalized environment order. -/
def layer
    (config : ConfigService)
    (github : GithubService)
    (store : IssueStore)
    (audit : Audit) : Layer Unit Empty Services :=
  let auditLayer : Layer Unit Empty Audit := Layer.succeed audit
  auditLayer.zipWith (rawLayer config github store)
    fun audit services => (audit, services)

end GithubIssueSync
