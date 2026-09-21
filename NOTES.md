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

21.09.

- Main goal is to have the graph based algorithms as part of the presolver:
- First algortihm: Reachability for ClusterBeforeUnlessFused. If we know their pis cant be fused, we can remove the constraint (or simplify) (graph only for pi order, pi are nodes and edges are order constraints)
- Splitting the problem into multiple ones as in the prototype should not be part of the presolver yet.
- Check for each constraint which implication they have for binary and order variables. (Default modules)
- Do some instrumentation parts of the benchmark in a limited amount of time.
- If time left, try replacing variables or look into the custom solver.

- For the custom solver, do presolving, branch on the ClusterBeforeUnlessFused constraints, and solve both. Take the one with the better cost according to the cost function.

- Find a paper for the colloqium talk.
- Make a separate PR for the accelerate build fix.
