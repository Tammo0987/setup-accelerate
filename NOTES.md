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

28.09.

## Done

- benchmark with phases -> presolve not a problem
- benchmarking with assign and replace -> no trend visible in cbc times result
- reachability for ClusterBeforeUnlessFused implemented
- prototype for custom solver implemented, not yet finished with the result. some ai prototypes did show the optimal solution, but implementation of approaches not yet finalized (needs more thinking about the approach)

## Not Done

- additional presolve graph phases
- build fix pr

## TODO

- double check fixpoint simplification (1. prio)
- default approach, (also order vars), setting a default value if there is no downside to it. (look experiment, one for binary and one for order variables)
- heuristic: how often does a var appears in constraint (2. prio)
- multiple separate problems. (3. prio)

05.10.

## Done

- Fixpoint simplification implemented.
- Implemented a default value approach for binary, but only minor improvements (can't really tell a difference, so dropped in favor of reduced complexity)
- Heuristic for variable frequency implemented, but not a global version. Only counts the set of in place variables and fusion, in this priority order. The other way around takes a lot longer (180s timeout vs 0.2665s). Reason not clear.
- Splitting the problem did show, that at least for sort we have one dominating problem (could just split to problems of size 1 and the rest), so there is no split which would be better than the original problem on its own. Dropped in favor of reduced complexity, but did show minor improvements.

- build fix pr
- found correctness bug which only did occur for other objectives. Boundaries were not respected.

- Found colloquium talk paper: egg
- Question: When is the official end date of the project and when should I start writing the report? Also, what are the requirements for the report?

## Not Done

- Implement a default values approach for order variables.

# TODO

- fixpoint benchmark?
- add binary default values from stash into the code.
- Split problems, try to do the singleton problems, but also, adapt the branch strategy to try to disconnect them.

- Add evaluation and future work for better grade.
- try different passes in the solver for benchmarks and show which part contributes how?
- skip the related work section, its a technical report not a paper.

- test separate inplace updates enabled and disabled. (set all inplace updates to false)

- colloquium talk, not next week and not the 9th november. schedule a date with alison. 14th december not possible as well.

- another idea: find articulation points adn branch on them. but sometimes its unbalanced. eval the cost for both branches and use the cost to upate the cost function and drop contraints.
- another idea: find "all" articulation points, sort them base on how even they distribute between the components and branch on the most even one.
- Not only articulation points, but also look for minimal cuts.

13.09.

## Done

- Tested different fixpoint variations, no significant difference in the results. The current implementation will be kept.
- Reworked benchmark into a more flexible framework for testing different variants.
- Added optional binary defaults with custom benchmark for comparison.
- Added optional direction var defaults with custom benchmark for comparison. For simple instances it creates overhead, but for more complext instances the combination is a small speedup.
