import Z

structure Issue

structure Comment extends Issue where 
  text: String

structure BusinessLogic where
  run: IO Unit --Z Unit IO.Error Unit

structure Github: Type 0 where
  getIssues (organization: String): IO (List Issue)
  postComment (issue: Issue) (comment: Comment): IO Unit

structure GithubZ: Type 1 where
  getIssues (organization: String): Z Unit IO.Error (List Issue)
  postComment (issue: Issue) (comment: Comment):  Z Unit IO.Error Unit


structure Http where
  get (url: String): IO ByteArray -- Z Unit IO.Error ByteArray
  post (url: String) (body: ByteArray): IO ByteArray -- Z Unit IO.Error ByteArray


def IO.getOrFail (v: Option A): IO A :=
  match v with
    | some a => pure a
    | none => throw (IO.userError "userError")

structure BusinessLogicLive extends BusinessLogic where
  github: Github
  run := do
    let issues <- github.getIssues "zio"
    let comment: Comment := {text := "Working on this!"}
    github.postComment (<- IO.getOrFail issues.head?) comment
    -- github.postComment (<- Z.getOrFail issues.head?) comment

def BusinessLogicLive.new (github: Github) := 
  { github := github: BusinessLogicLive}

instance : Coe BusinessLogicLive BusinessLogic := ⟨BusinessLogicLive.toBusinessLogic⟩

-- structure GithubLive extends Github where
--   http: Http := sorry
--   getIssues org := sorry
--   postComment issue comment := sorry

-- def GithubLive.new (http: Http) := 
--   { http := http : GithubLive }

-- instance : Coe GithubLive Github := ⟨GithubLive.toGithub⟩

structure HttpConfig

-- structure HttpLive extends Http where
--   config : HttpConfig
--   get url := sorry
--   post url body := sorry
--   start : IO Unit := sorry
--   shutdown : IO Unit := sorry

-- def HttpLive.new (config: HttpConfig): HttpLive := { config := config}

-- instance : Coe HttpLive Http := ⟨HttpLive.toHttp⟩

-- def mainZ := 
--   let http: Http := HttpLive.new sorry
--   let github := GithubLive.new http
--   let businessLogic: BusinessLogic := BusinessLogicLive.new github
--   businessLogic.run


/- Using Layers -/

/- Problem: Only services with of Type 0 can be used! -/  

-- https://leanprover.zulipchat.com/#narrow/stream/270676-lean4/topic/universe.20polymorphic.20IO
-- https://github.com/leanprover/lean4/issues/1136

-- https://leanprover.zulipchat.com/#narrow/stream/236449-Program-verification/topic/universes/near/257719653


def BusinessLogicLive.layer: Layer Github Empty BusinessLogic := 
  (Layer.fromFunction BusinessLogicLive.new).map (Environment.map BusinessLogicLive.toBusinessLogic)


-- def GithubLive.layer: Layer Http Empty Github := 
--   (Layer.fromFunction GithubLive.new).map (Environment.map GithubLive.toGithub)


-- def HttpLive.layer: Layer HttpConfig IO.Error Http := 
--   Layer.fromZ do
--     let config <- Z.service HttpConfig
--     let http <- Z.succeedNow' (HttpLive.new config)
--     Z.attempt' http.start
--     -- Z.addFinalizer http.shutdown
--     return http



-- class HBind (m : Type u1 → Type v1) (n : Type u2 → Type v2) where
--   hBind {α : Type u1} {β : Type u2} : m α → (α → n β) → n β

-- @[defaultInstance] instance [Bind m] : HBind m m := ⟨bind⟩
