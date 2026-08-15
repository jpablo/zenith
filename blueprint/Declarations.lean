/-
Declaration checks for the Zenith Formalization Blueprint.

Run from the project root:

    lake env lean blueprint/Declarations.lean

`leanblueprint web` also writes `blueprint/lean_decls` from the `\\lean`
annotations in the TeX source. This file is checked without adding the
optional `checkdecls` Lake dependency.
-/

import Zenith.Formalization.TypeAlgebra
import Zenith.Formalization.ServiceRowConnection
import Zenith.Formalization.ErrorShape
import Zenith.Formalization.VarianceLaws
import Zenith.Formalization.SequentialCore
import Zenith.Formalization.SequentialMachine
import Zenith.Formalization.SequentialRuntimeStack
import Zenith.Formalization.SequentialRuntime
import Zenith.Formalization.SequentialDispatcher

#check Zenith.Formalization.TypeAlgebra.Requirement.and_isGreatestLowerBound
#check Zenith.Formalization.TypeAlgebra.Requirement.normalForm_eq_of_equivalent
#check Zenith.Formalization.TypeAlgebra.ErrorType.or_isLeastUpperBound
#check Zenith.Formalization.TypeAlgebra.ErrorType.normalForm_eq_of_equivalent
#check Zenith.Formalization.ServiceRows.canProvide_provides
#check Zenith.Formalization.ServiceRows.canProvide_nonempty_of_provides
#check Zenith.Formalization.ErrorShape.joinUpperBound
#check Zenith.Formalization.VarianceLaws.flatMapMeetJoin
#check Zenith.Formalization.SequentialCore.evaluates_deterministic
#check Zenith.Formalization.SequentialMachine.evaluation_runs_to_halt
#check Zenith.Formalization.SequentialRuntimeStack.corresponding_size
#check Zenith.Formalization.SequentialRuntime.step_refines
#check Zenith.Formalization.SequentialRuntime.steps_refine
#check Zenith.Formalization.SequentialDispatcher.run_models_step
#check Zenith.Formalization.SequentialDispatcher.success_models_step
#check Zenith.Formalization.SequentialDispatcher.failure_models_step
