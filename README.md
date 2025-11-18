# CALC Language Compiler

A compiler for the CALC expression language, featuring arithmetic, boolean operations, variables, references, and imperative constructs. This project includes both an interpreter and a compiler targeting LLVM IR.

## Implemented Features

### Core Language (Labs 1-4)

- **Literals**: Integers, booleans, unit value `()`
- **Arithmetic**: `+`, `-`, `*`, `/`, unary `-`
- **Comparisons**: `=`, `!=`, `<`, `<=`, `>`, `>=`
- **Boolean Logic**: `&&`, `||`, `not` (with short-circuit evaluation)

### Variables & Scope (Lab 5)

- **Let Bindings**: `let x = e1 in e2`
- **Multiple Bindings**: `let x = e1 and y = e2 in e3`
- **Lexical Scoping**: Environment management

### Imperative Features (Lab 7)

- **References**: `new(e)`, `!e`, `e1 := e2`, `free(e)`
- **Control Flow**: `if e1 then e2 else e3`, `while e1 do e2`
- **Sequencing**: `e1; e2`
- **I/O**: `printInt(e)`, `printBool(e)`, `printEndLine()`

## Architecture

- **Lexer** (`lexer.mll`): Tokenization
- **Parser** (`parser.mly`): Syntax analysis with precedence rules
- **Type Checker** (`typing.ml`): Static type checking with `int`, `bool`, `unit`, `ref T`
- **Interpreter** (`eval.ml`): Direct evaluation with OCaml references
- **LLVM Compiler** (`llvm.ml`): Code generation targeting LLVM IR
- **Runtime** (`mem_runtime.c`): C runtime for heap memory management

## Building & Running

```bash
# Build
dune build

# Run interpreter
dune exec calc

# Run compiler
dune exec calcc

# Compile generated LLVM
clang -c mem_runtime.c -o mem_runtime.o
clang prog.ll mem_runtime.o -o prog
./prog
```

## Example Program

```ocaml
let x = new(0) in
while !x < 5 do (
  printInt(!x);
  x := !x + 1
);
printEndLine()
```

## Thank you

**Authors**:

- Wiktor Szydłowski 75135
- Valerii Matviiv (REMEMBER TO FILL IN YOUR STUDENT ID)

---

> *“There are only two kinds of languages: the ones people complain about and the ones nobody uses.”*
>
> **Bjarne Stroustrup**

---

[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/nn2FV4D7)