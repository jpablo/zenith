import Z.KeyedLayerMake

/-!
A complete command-line application built with Zenith.

The application scans a workspace for `TODO`, `FIXME`, and `HACK` markers and
writes a Markdown report. Its dependency graph contains configuration, file
system, scanner, writer, and console services. The live executable and the
in-memory integration tests use the same program.
-/

namespace TodoReport

open System
open Z

structure Arguments : Type 1 where
  values : List String
  deriving ServiceKey

structure AppConfig : Type 1 where
  root : FilePath
  output : FilePath
  markers : List String
  extensions : List String
  excludedDirectories : List String
  deriving ServiceKey

inductive ConfigError where
  | usage (message : String)
  deriving BEq, Repr

instance : ToString ConfigError where
  toString
    | .usage message => message

inductive FileError where
  | operationFailed
      (operation : String)
      (path : FilePath)
      (message : String)
  deriving BEq, Repr

instance : ToString FileError where
  toString
    | .operationFailed operation path message =>
        s!"{operation} failed for '{path}': {message}"

abbrev AppError := ConfigError ⊕ FileError

structure FileEntry where
  path : FilePath
  name : String
  isDirectory : Bool
  deriving BEq, Repr

structure Finding where
  path : FilePath
  line : Nat
  marker : String
  text : String
  deriving BEq, Repr

structure ScanResult where
  filesScanned : Nat := 0
  findings : List Finding := []
  deriving BEq, Repr, Inhabited

private instance [Inhabited A] : Inhabited (Z R E A) where
  default := Z.internal.succeedNow default

namespace ScanResult

def combine (left right : ScanResult) : ScanResult := {
  filesScanned := left.filesScanned + right.filesScanned
  findings := left.findings ++ right.findings
}

end ScanResult

structure FileSystem : Type 1 where
  listDirectory : FilePath -> Z Unit FileError (List FileEntry)
  readFile : FilePath -> Z Unit FileError String
  writeFile : FilePath -> String -> Z Unit FileError Unit
  deriving ServiceKey

structure TodoScanner : Type 1 where
  scan : AppConfig -> Z Unit FileError ScanResult
  deriving ServiceKey

structure ReportWriter : Type 1 where
  write : AppConfig -> ScanResult -> Z Unit FileError Unit
  deriving ServiceKey

structure Console : Type 1 where
  printLine : String -> Z Unit Empty Unit
  deriving ServiceKey

structure TodoApp : Type 1 where
  run : Z Unit FileError ScanResult
  deriving ServiceKey

def defaultConfig (root : FilePath) (output : FilePath) : AppConfig := {
  root
  output
  markers := ["TODO:", "FIXME:", "HACK:"]
  extensions := ["lean", "md"]
  excludedDirectories := [".git", ".lake", "build"]
}

def parseConfig (rawArguments : List String) : Except ConfigError AppConfig :=
  let arguments := match rawArguments with
    | "--" :: rest => rest
    | _ => rawArguments
  match arguments with
  | [] =>
      let root : FilePath := "."
      .ok (defaultConfig root (root / "todo-report.md"))
  | [rootText] =>
      let root : FilePath := rootText
      .ok (defaultConfig root (root / "todo-report.md"))
  | [rootText, outputText] =>
      let root : FilePath := rootText
      let output : FilePath := outputText
      let output :=
        if output.isRelative then root / outputText else output
      .ok (defaultConfig root output)
  | _ =>
      .error (ConfigError.usage
        "usage: lake exe todoReport [ROOT] [OUTPUT]\nROOT defaults to '.', and OUTPUT defaults to ROOT/todo-report.md")

private def firstMarker
    (markers : List String)
    (line : String) : Option String :=
  markers.find? fun marker => line.contains marker

def scanContent
    (config : AppConfig)
    (path : FilePath)
    (content : String) : List Finding :=
  (content.splitOn "\n").zipIdx.filterMap fun (text, index) =>
    (firstMarker config.markers text).map fun marker => {
      path
      line := index + 1
      marker
      text := text.trimAscii.toString
    }

private def isSourceFile (config : AppConfig) (path : FilePath) : Bool :=
  match path.extension with
  | some extension => config.extensions.contains extension
  | none => false

private def sortedEntries (entries : List FileEntry) : List FileEntry :=
  entries.mergeSort fun left right =>
    left.path.toString <= right.path.toString

partial def scanDirectory
    (fileSystem : FileSystem)
    (config : AppConfig)
    (directory : FilePath) : Z Unit FileError ScanResult := zdo
  let entries <- fileSystem.listDirectory directory
  let mut result := {}
  for entry in sortedEntries entries do
    if entry.isDirectory then
      if !config.excludedDirectories.contains entry.name then
        let nested <- scanDirectory fileSystem config entry.path
        result := result.combine nested
    else if entry.path.toString != config.output.toString &&
        isSourceFile config entry.path then
      let content <- fileSystem.readFile entry.path
      result := result.combine {
        filesScanned := 1
        findings := scanContent config entry.path content
      }
  pure result

def renderReport (config : AppConfig) (result : ScanResult) : String :=
  let summary : String :=
    s!"# TODO Report\n\n- Root: `{config.root}`\n- Files scanned: {result.filesScanned}\n- Findings: {result.findings.length}\n\n## Findings\n\n"
  if result.findings.isEmpty then
    summary ++ "No markers found.\n"
  else
    let lines : List String := result.findings.map fun finding =>
      s!"- `{finding.path}:{finding.line}` **{finding.marker}** — " ++
      finding.text
    String.append summary <|
      String.append (String.intercalate "\n" lines) "\n"

private def ioOperation
    (operation : String)
    (path : FilePath)
    (action : IO A) : Z Unit FileError A :=
  (Z.attempt action).mapFailure fun error =>
    .operationFailed operation path (toString error)

def liveFileSystem : FileSystem := {
  listDirectory := fun path => ioOperation "list directory" path do
    let entries <- path.readDir
    let result <- entries.toList.mapM fun entry => do
      let metadata <- entry.path.symlinkMetadata
      pure {
        path := entry.path
        name := entry.fileName
        isDirectory := metadata.type == .dir
      }
    pure result
  readFile := fun path =>
    ioOperation "read file" path (IO.FS.readFile path)
  writeFile := fun path content =>
    ioOperation "write file" path (IO.FS.writeFile path content)
}

def liveConsole : Console := {
  printLine := fun message => Z.succeed (IO.println message)
}

def configLayer :
    KeyedLayer
      (Services[Arguments])
      ConfigError
      (ServiceRow[AppConfig]) :=
  KeyedLayer.fromLayer (Layer.fromHEIO fun environment =>
    let arguments := Services.get[Arguments] environment
    match parseConfig arguments.values with
    | .ok config => HEIO.pure config
    | .error error => HEIO.throw (.fail error))

def scannerLayer :
    KeyedLayer
      (Services[FileSystem])
      Empty
      (ServiceRow[TodoScanner]) :=
  KeyedLayer.fromLayer (Layer.fromFunction fun environment =>
    let fileSystem := Services.get[FileSystem] environment
    {
      scan := fun config =>
        scanDirectory fileSystem config config.root
    })

def writerLayer :
    KeyedLayer
      (Services[FileSystem])
      Empty
      (ServiceRow[ReportWriter]) :=
  KeyedLayer.fromLayer (Layer.fromFunction fun environment =>
    let fileSystem := Services.get[FileSystem] environment
    {
      write := fun config result =>
        fileSystem.writeFile config.output (renderReport config result)
    })

def appLayer :
    KeyedLayer
      (Services[AppConfig, Console, ReportWriter, TodoScanner])
      Empty
      (ServiceRow[TodoApp]) :=
  KeyedLayer.fromLayer (Layer.fromFunction fun environment =>
    let config := Services.get[AppConfig] environment
    let console := Services.get[Console] environment
    let writer := Services.get[ReportWriter] environment
    let scanner := Services.get[TodoScanner] environment
    {
      run := zdo
        let result <- scanner.scan config
        let _ <- writer.write config result
        let message : String :=
          s!"Wrote {result.findings.length} findings from {result.filesScanned} files to {config.output}."
        let _ <- console.printLine message
        pure result
    })

def program :
    Z (Services[TodoApp]) FileError ScanResult :=
  Z.serviceWithZ[TodoApp] fun app => app.run

def application
    (arguments : Arguments)
    (fileSystem : FileSystem)
    (console : Console) :
    Z (Services[]) AppError ScanResult :=
  Z.provide program [
    appLayer,
    scannerLayer,
    writerLayer,
    configLayer,
    KeyedLayer.succeed arguments,
    KeyedLayer.succeed fileSystem,
    KeyedLayer.succeed console
  ]

def liveApplication (arguments : List String) :
    Z (Services[]) AppError ScanResult :=
  application { values := arguments } liveFileSystem liveConsole

/-! ## In-memory integration checks -/

structure MemoryFileSystem where
  directories : List (FilePath × List FileEntry)
  files : List (FilePath × String)
  writes : IO.Ref (List (FilePath × String))

private def findDirectory
    (directories : List (FilePath × List FileEntry))
    (path : FilePath) : Option (List FileEntry) :=
  (directories.find? fun item =>
    item.1.toString == path.toString).map Prod.snd

private def findFile
    (files : List (FilePath × String))
    (path : FilePath) : Option String :=
  (files.find? fun item =>
    item.1.toString == path.toString).map Prod.snd

def memoryFileSystem (memory : MemoryFileSystem) : FileSystem := {
  listDirectory := fun path =>
    match findDirectory memory.directories path with
    | some entries => Z.succeedNow entries
    | none => Z.fail (FileError.operationFailed
        "list directory" path "directory not found")
  readFile := fun path =>
    match findFile memory.files path with
    | some content => Z.succeedNow content
    | none => Z.fail (FileError.operationFailed
        "read file" path "file not found")
  writeFile := fun path content =>
    Z.succeed <| memory.writes.modify fun writes =>
      writes ++ [(path, content)]
}

def memoryConsole (messages : IO.Ref (List String)) : Console := {
  printLine := fun message =>
    Z.succeed <| messages.modify fun current => current ++ [message]
}

private def check (message : String) (condition : Bool) : IO Unit := do
  unless condition do
    throw (IO.userError message)

private def runTestApplication
    (name : String)
    (effect : Z (Services[]) AppError ScanResult) :
    IO (Exit AppError ScanResult) := do
  let closed := effect.provideEnvironment Z.Services.empty
  Z.unsafeRunSync closed name

def test : IO Unit := do
  match parseConfig [".", "todo-report.md"] with
  | .ok config =>
      check "TODO report did not resolve a relative output from its root"
        (config.output.toString == "./todo-report.md")
  | .error _ =>
      throw (IO.userError "TODO report rejected valid relative paths")

  let writes <- IO.mkRef ([] : List (FilePath × String))
  let messages <- IO.mkRef ([] : List String)
  let root : FilePath := "/workspace"
  let output : FilePath := "/workspace/todo-report.md"
  let memory : MemoryFileSystem := {
    directories := [
      (root, [
        { path := root / ".git", name := ".git", isDirectory := true },
        { path := root / "README.md", name := "README.md", isDirectory := false },
        { path := root / "src", name := "src", isDirectory := true }
      ]),
      (root / "src", [
        { path := root / "src" / "Ignored.bin", name := "Ignored.bin", isDirectory := false },
        { path := root / "src" / "Main.lean", name := "Main.lean", isDirectory := false }
      ])
    ]
    files := [
      (root / "README.md", "# Notes\nFIXME: improve the guide\n"),
      (root / "src" / "Main.lean",
        "def ready := true\n-- TODO: add the command\n-- HACK: temporary path\n")
    ]
    writes
  }
  let effect := application
    { values := [root.toString, output.toString] }
    (memoryFileSystem memory)
    (memoryConsole messages)
  match <- runTestApplication "todo-report-memory" effect with
  | .success result =>
      check "TODO report scanned the wrong file count"
        (result.filesScanned == 2)
      check "TODO report found the wrong marker count"
        (result.findings.length == 3)
  | .failure cause =>
      throw (IO.userError s!"TODO report failed: {cause}")
  match <- writes.get with
  | [(path, report)] =>
      check "TODO report used the wrong output path"
        (path.toString == output.toString)
      check "TODO report did not contain the summary"
        (report.contains "Files scanned: 2")
      check "TODO report did not contain a nested finding"
        (report.contains "/workspace/src/Main.lean:2")
      check "TODO report scanned its excluded directory"
        (!report.contains ".git")
  | _ => throw (IO.userError "TODO report did not write exactly one report")
  check "TODO report did not print its completion message"
    ((<- messages.get).length == 1)

  let invalid := application
    { values := ["one", "two", "three"] }
    (memoryFileSystem memory)
    (memoryConsole messages)
  match <- runTestApplication "todo-report-invalid-arguments" invalid with
  | .failure (.fail (.inl (.usage _))) => pure ()
  | _ => throw (IO.userError "TODO report accepted invalid arguments")

  let missingFileMemory : MemoryFileSystem := {
    directories := [
      (root, [
        { path := root / "Missing.lean", name := "Missing.lean", isDirectory := false }
      ])
    ]
    files := []
    writes
  }
  let missingFile := application
    { values := [root.toString, output.toString] }
    (memoryFileSystem missingFileMemory)
    (memoryConsole messages)
  match <- runTestApplication "todo-report-missing-file" missingFile with
  | .failure (.fail (.inr (.operationFailed "read file" path _))) =>
      check "TODO report returned the wrong failed path"
        (path.toString == (root / "Missing.lean").toString)
  | _ => throw (IO.userError "TODO report did not expose a read failure")

end TodoReport
