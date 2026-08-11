import Examples.GithubIssueSync

namespace GithubIssueSyncDemo

open GithubIssueSync
open Z

def printStep (message : String) : Z Unit Empty Unit :=
  Z.succeed (IO.println message)

def loadConfig : Z Unit ConfigError SyncConfig := zdo
  let _ <- printStep "[config] load"
  pure {
    organization := "lean"
    dryRun := false
  }

def openIssues (organization : String) : Z Unit GithubError (List Issue) := zdo
  let _ <- printStep s!"[github] get open issues for {organization}"
  pure [
    { id := 101, title := "Improve error messages" },
    { id := 102, title := "Document layer composition" }
  ]

def saveIssue (issue : Issue) : Z Unit StoreError Unit := zdo
  let _ <- printStep s!"[store] save #{issue.id}: {issue.title}"
  pure ()

def recordFailure (message : String) : Z Unit AuditError Unit := zdo
  let _ <- printStep s!"[audit] failure: {message}"
  pure ()

def finishAudit : Z Unit Empty Unit :=
  printStep "[audit] finish"

def configService : ConfigService := {
  load := loadConfig
}

def githubService : GithubService := {
  openIssues := openIssues
}

def issueStore : IssueStore := {
  save := saveIssue
}

def audit : Audit := {
  recordFailure := recordFailure
  finish := finishAudit
}

def demoApplication :=
  GithubIssueSync.application configService githubService issueStore audit

example : Z (Services[]) Empty Nat :=
  demoApplication

def run : IO Unit := do
  let effect := demoApplication.provideEnvironment
    Z.Services.empty
  match <- Z.unsafeRunSync effect "github-issue-sync-demo" with
  | .success count =>
      IO.println s!"Synchronized {count} issues."
  | .failure _ =>
      throw (IO.userError "The GitHub issue sync failed.")

end GithubIssueSyncDemo

def main : IO Unit :=
  GithubIssueSyncDemo.run
