# Modifications to the `monitor` file

 - Modified `Scope` file to store assertions in a scope object
 - Moved type-checking from `STInterpreter` to new file `STSolver`
 - Modified `Synth` file to call the new extension  
 - Added the following files:
   - `STSolver` - type-checks, invokes the solver algorithm and optimises session type
   - `STSolverZ3` - contains code regarding the satisfiability checks
   - `STSolverHelper` - contains helper functions for the tool

