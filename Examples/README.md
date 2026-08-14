# Example programs

This folder contains small and complete Zenith programs. Most files define an
example, and files with the `Main` or `Demo` suffix provide a Lake executable
entry point.

* `Basic.lean` demonstrates core effects, errors, fibers, and `zdo`.
* `ErrorHandling.lean` demonstrates typed failures and recovery.
* `GithubIssueSync.lean` and `GithubIssueSyncDemo.lean` define and run an
  issue-sync application.
* `HttpServer.lean` demonstrates the optional `Zenith.Http` adapter.
* `OnionArchitecture.lean` demonstrates service boundaries and layers.
* `ScopedResourceDemo.lean` demonstrates safe resource lifetime management.
* `QueueWorkerPool.lean` and `QueueWorkerPoolMain.lean` demonstrate bounded
  concurrent work with `Z.Queue`.
* `StableServiceKeysDemo.lean` and `StableServiceKeysDemoMain.lean` demonstrate
  keyed services and automatic layer composition.
* `TodoReport.lean` and `TodoReportMain.lean` implement a useful Markdown TODO
  report program.

The root README lists the Lake commands that run the standalone examples.
