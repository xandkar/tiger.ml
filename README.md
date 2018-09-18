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
In order to support mutual recursion, we need to group consecutive
type and function declarations (see Tiger-book pages 97-99).

Initially, I defined the rules to do so as:

    decs:
      | dec      { $1 :: [] }
      | dec decs { $1 :: $2 }
      ;
    dec:
      | var_dec  { $1 }
      | typ_decs { Ast.TypeDecs $1 }
      | fun_decs { Ast.FunDecs $1 }
      ;

which, while straightforward (and working, because `ocamlyacc` defaults to
shift in case of a conflict), nonetheless caused a shift/reduce conflict in
each of: `typ_decs` and `fun_decs`; where the parser did not know whether to
shift and stay in `(typ|fun_)_dec` state or to reduce and get back to `dec`
state.

Sadly, tagging the rules with a lower precedence (to explicitly favor
shifting) - does not help :(

    %nonassoc LOWEST
    ...
    dec:
      | var_dec                { $1 }
      | typ_decs  %prec LOWEST { Ast.TypeDecs $1 }
      | fun_decs  %prec LOWEST { Ast.FunDecs $1 }
      ;

The difficulty seems to be in the lack of a separator token which would be
able to definitively mark the end of each sequence of consecutive
`(typ_|fun_)` declarations.

Keeping this in mind, another alternative is to manually capture the possible
interspersion patterns in the rules like:

    (N * foo) followed-by (N * not-foo)

for the exception of `var_dec`, which, since we do not need to group its
consecutive sequences, can be reduced upon first sighting.

The final rules I ended-up with are:

    decs:
      | var_dec   decs_any          { $1 :: $2 }
      | fun_decs  decs_any_but_fun  { (Ast.FunDecs  $1) :: $2 }
      | typ_decs  decs_any_but_typ  { (Ast.TypeDecs $1) :: $2 }
      ;

    decs_any:
      |                             { [] }
      | var_dec   decs_any          { $1 :: $2 }
      | fun_decs  decs_any_but_fun  { (Ast.FunDecs  $1) :: $2 }
      | typ_decs  decs_any_but_typ  { (Ast.TypeDecs $1) :: $2 }
      ;

    decs_any_but_fun:
      |                             { [] }
      | var_dec   decs_any          { $1 :: $2 }
      | typ_decs  decs_any_but_typ  { (Ast.TypeDecs $1) :: $2 }
      ;

    decs_any_but_typ:
      |                             { [] }
      | var_dec   decs_any          { $1 :: $2 }
      | fun_decs  decs_any_but_fun  { (Ast.FunDecs $1) :: $2 }
      ;

##### lval

### AST

#### print as M-exp

I chose to pretty-print AST as an (indented)
[M-expression](https://en.wikipedia.org/wiki/M-expression) - an underrated
format, used in Mathematica and was intended for Lisp by McCarthy himself; it
is nearly as flexible as S-expressions, but significantly more readable (IMO).

As an example, here is what `test28.tig` looks like after parsing and
pretty-printing:

    LetExp[
        [
        TypeDecs[
            TypeDec[
                arrtype1,
                ArrayTy[
                int]],
            TypeDec[
                arrtype2,
                ArrayTy[
                int]]],
        VarDec[
            arr1,
            arrtype1,
            ArrayExp[
                arrtype2,
                IntExp[
                    10],
                IntExp[
                    0]]]],
        SeqExp[
            VarExp[
                SimpleVar[
                    arr1]]]]

### Machine
Will most-likely compile to RISC and execute using SPIM (as favored by Appel)
