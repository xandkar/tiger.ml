Tiger.ml
========
A Tiger-compiler implementation in (OCa)ML

Status
------

![screenshot-tests-head](screenshots/tests-head.jpg)
...
![screenshot-tests-tail](screenshots/tests-tail.jpg)

### Features
#### Done
- [x] ch 1: Warm-up AST
- [x] ch 2: Lexer
- [x] ch 3: Parser
- [x] ch 4: AST
- [x] ch 5: Semantic Analysis (type checking)
#### In-progress
- [ ] ch 6: Activation Records
#### TODO (short-term)
- [ ] ch 7: Translation to Intermediate Code
- [ ] ch 08: Basic Blocks and Traces
- [ ] ch 09: Instruction Selection
- [ ] ch 10: Liveness Analysis
- [ ] ch 11: Register Allocation
- [ ] ch 12: Putting It All Together
#### TODO (long-term)
- [ ] ch 13: Garbage Collection
- [ ] ch 15: Functional Programming Languages
- [ ] ch 16: Polymorphic Types
- [ ] ch 17: Dataflow Analysis
- [ ] ch 18: Loop Optimizations
- [ ] ch 19: Static Single-Assignment Form
- [ ] ch 20: Pipelining and Scheduling
- [ ] ch 21: The Memory Hierarchy
#### Maybe
- [ ] ch 14: Object-Oriented Languages

### Technical issues
- [-] testing framework
  - [x] run arbitrary code snippets
  - [x] check non-failures
  - [x] check expected output
  - [-] check expected exceptions
    - [x] semant stage
    - [ ] generalized expect `Output ('a option) | Exception of (exn -> bool)`
  - [x] run all book test case files 
  - [-] grid view (cols: lex, pars, semant, etc.; rows: test cases.) 
    - [x] implementation
    - [ ] refactoring
  - [ ] test time-outs (motive: cycle non-detection caused an infinite loop)
    - [ ] parallel test execution
- [ ] Travis CI

Implementation Notes
--------------------

### Parser

#### shift/reduce conflicts
##### grouping consecutive declarations
##### lval

### AST

#### print as M-exp

### Machine
Will most-likely compile to RISC and execute using SPIM (as favored by Appel)
