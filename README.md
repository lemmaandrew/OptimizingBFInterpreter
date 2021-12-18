# OptimizingBFInterpreter

A Brainfuck interpreter that performs optimizations.

The optimizations include

* Merging addition and subtraction operations
* Merging pointer shifting operations
* Converting `[-]` and `[+]` to `Clear` commands
* Converting loops like `[->+>>+++<<<]` to `Copy` commands

This interpreter also utilizes mutable vectors and monad transformers for fast read/write operations.
