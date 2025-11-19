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

### First-Class Functions

- **Lambda Functions**: `fun (param: type) -> expr`
- **Function Application**: `f(arg)`
- **Function Types**: `type1 -> type2`
- **Closures**: Capture environment where functions are defined
- **Higher-Order Functions**: Functions as arguments and return values

## Architecture

- **Lexer** (`lexer.mll`): Tokenization
- **Parser** (`parser.mly`): Syntax analysis with precedence rules
- **AST** (`ast.ml`): Abstract syntax tree with type annotations
- **Type Checker** (`typing.ml`): Static type checking with `int`, `bool`, `unit`, `ref T`, `T1 -> T2`
- **Interpreter** (`eval.ml`): Direct evaluation with OCaml closures
- **LLVM Compiler** (`llvm.ml`): Code generation targeting LLVM IR
- **Runtime** (`mem_runtime.c`, `closure_runtime.c`): C runtime for memory and closure management
- **Environment** (`env.ml`): Scoped environment for variable bindings

## Building Running & Testing in the programs/ folder

```bash
# Build
make build

# Run interpreter (interactive REPL)
make repl

# Compile and run a program
make run FILE=program.calc

## Running Tests and Optimizations
You can run individual programs or the full test suite. By default, optimizations are **disabled**.

# Run standard test suite
make test

# Run test suite with optimizations enabled
make test OPT=1

# Clean
make clean
```

## Usage

Put your `.calc` files in `programs/` directory, then:

```bash
make run FILE=myprogram.calc
```

All build artifacts go to `build/` directory.

## Example Programs

### Basic Function

```ocaml
let double = fun (x: int) -> x * 2 in
printInt(double(21));
printEndLine()
```

### Imperative Loop

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
- Valerii Matviiv 75176 

---

> *"There are only two kinds of languages: the ones people complain about and the ones nobody uses."*
>
> **Bjarne Stroustrup**

---

[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/nn2FV4D7)