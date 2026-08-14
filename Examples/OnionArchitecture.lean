import Z.KeyedLayerMake

/-!
A small onion-style application built with the current keyed service API.

The program depends only on `BusinessLogic`. The automatic `Z.provide` graph
builds that service from `Github`, then builds `Github` from `HttpClient`, and
finally builds `HttpClient` from `HttpConfig`.
-/

namespace OnionArchitecture

open Z

structure Issue where
  id : Nat
  title : String
  deriving Repr

structure Comment where
  text : String
  deriving Repr

inductive HttpError where
  | unavailable (path : String)
  deriving Repr

instance : ToString HttpError where
  toString
    | .unavailable path => s!"HTTP service unavailable: {path}"

structure HttpConfig : Type 1 where
  baseUrl : String
  deriving ServiceKey

structure HttpClient : Type 1 where
  get : String -> Z Unit HttpError String
  post : String -> String -> Z Unit HttpError Unit
  deriving ServiceKey

structure Github : Type 1 where
  getIssues : String -> Z Unit HttpError (List Issue)
  postComment : Issue -> Comment -> Z Unit HttpError Unit
  deriving ServiceKey

structure BusinessLogic : Type 1 where
  run : Z Unit HttpError Unit
  deriving ServiceKey

def makeHttpClient (config : HttpConfig) : HttpClient := {
  get := fun path => Z.fromIO do
    IO.println s!"GET {config.baseUrl}{path}"
    pure "ok"
  post := fun path body => Z.fromIO do
    IO.println s!"POST {config.baseUrl}{path}: {body}"
}

def httpClientLayer :=
  KeyedLayer.derive makeHttpClient

def makeGithub (http : HttpClient) : Github := {
  getIssues := fun organization => zdo
    let _ <- http.get s!"/orgs/{organization}/issues"
    pure [
      { id := 1, title := "Update the examples" },
      { id := 2, title := "Document automatic layers" }
    ]
  postComment := fun issue comment =>
    http.post s!"/issues/{issue.id}/comments" comment.text
}

def githubLayer :=
  KeyedLayer.derive makeGithub

def makeBusinessLogic (github : Github) : BusinessLogic := {
  run := zdo
    let issues <- github.getIssues "leanprover"
    for issue in issues do
      github.postComment issue {
        text := s!"Working on: {issue.title}"
      }
}

def businessLogicLayer :=
  KeyedLayer.derive makeBusinessLogic

def program : Z (Services[BusinessLogic]) HttpError Unit :=
  Z.serviceWithM[BusinessLogic] fun businessLogic =>
    businessLogic.run

/-- Compose the complete dependency graph from its layer candidates. -/
def application (config : HttpConfig) :=
  Z.provide program [
    businessLogicLayer,
    githubLayer,
    httpClientLayer,
    KeyedLayer.succeed config
  ]

example (config : HttpConfig) :
    Z (Services[]) HttpError Unit :=
  application config

def demoConfig : HttpConfig := {
  baseUrl := "https://api.github.test"
}

def demoApplication : Z (Services[]) HttpError Unit :=
  application demoConfig

def runnableDemo : Z Unit HttpError Unit :=
  demoApplication.provideEnvironment
    Z.Services.empty

end OnionArchitecture
