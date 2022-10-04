import Z.Util

structure Metadata where
  label : String
  nodeId : NodeId := ""
  deriving Repr

def Metadata.empty : Metadata where
  label  := ""
  nodeId := ""

def mempty := Metadata.empty
def Metadata.withLabel (label : String) : Metadata :=  { label }

