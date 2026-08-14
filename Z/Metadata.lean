import Z.Util

/-- Optional execution-tracing information attached to an instruction. -/
structure Metadata where
  label : String
  nodeId : NodeId := ""
  deriving Repr

/-- Metadata with no label or trace-node identifier. -/
def Metadata.empty : Metadata where
  label  := ""
  nodeId := ""

/-- The default metadata value used by instruction constructors. -/
def mempty := Metadata.empty

/-- Metadata with `label` and an empty trace-node identifier. -/
def Metadata.withLabel (label : String) : Metadata :=  { label }
