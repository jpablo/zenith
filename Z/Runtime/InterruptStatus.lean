
/-- Whether the interpreter may deliver a pending interruption. -/
inductive InterruptStatus where
  | interruptible 
  | uninterruptible

instance : ToString InterruptStatus where
  toString s := match s with
  | .interruptible => "interruptible"
  | .uninterruptible => "uninterruptible"

/-- Convert an interruption status to the interpreter's Boolean representation. -/
def InterruptStatus.toBool: InterruptStatus -> Bool
  | interruptible   => true
  | uninterruptible => false
