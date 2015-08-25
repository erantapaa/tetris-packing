A branch-and-bound algorithm to pack Tetris pieces into a grid.

Problems in the `data/` directory are from

https://github.com/instructure/contests/tree/master/2012-mebipenny/contest/kyles-kitchen

Run the branch-and-bound solver on a problem:

    ghci> runSolver solve1 "2 4 C C"

Run the DFS solver:

    ghci> runSolver solve2 "2 4 C C"

Solve a problem from a file:

    ghci> solveFile solve1 "data/stdin6.txt"

You can see the effect the the branch-and-bound check by changing
this line in `search1`:

    if canPrune

to:

    if trace msg canPrune

