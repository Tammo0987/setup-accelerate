# Notes

14.09.

- Small PR with fix of bounds for fusion variables including an example to reproduce.
- compilation time is long with multiple sorting operation (accelerate containers) to benchmark.
- Use criterion to benchmark. Use run1 or runN to only measure the time of compilation. Regalur cfal-bench (paper)
- Use timebox for benchmark, do not spend to much on it on.
- Maybe extend the set of test programs. Add some more none trivial programs to verify correctness.
- Maybe its neccessary to update the cost function, there are two ways, trcking substition or actually modfying.
- Maybe we need to change the nodes with variables in the constraint language if more helpful in the solver.
- Idea function signature: substitions x constraint -> substitions x List[Constraint]. Repeat until no new substitions are found.
- Substistute variables with another if we have an assignment, to reduce the amount of variables.
- Vars vs Nodes, same amount of information, use whatever is easier to write down. Alternative, I could introduce another custom variable data type with a mapping to the current variable type.
- Double check cluster order constraints.

## For the solver
- idea: use order constraints -> encode them into an fgl graph. 
- use reasoning e.g., if we find a cycle, it must be fused.
