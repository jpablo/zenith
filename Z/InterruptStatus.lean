
inductive InterruptStatus where
  | interruptible 
  | uninterruptible

instance : ToString InterruptStatus where
  toString s := match s with
  | .interruptible => "interruptible"
  | .uninterruptible => "uninterruptible"

def InterruptStatus.toBool: InterruptStatus -> Bool
  | interruptible   => true
  | uninterruptible => false