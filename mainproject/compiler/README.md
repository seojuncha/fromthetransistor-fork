## Compiler (WIP)

This directory contains an experimental C compiler written in Common Lisp (SBCL). It is a part of the project’s goal to build a full toolchain from scratch — from source code to binary execution on a custom MCU.

## Design Goals

- Write a minimal ISO C99-compatible compiler targeting our own instruction set
- Build the compiler in Common Lisp for rapid prototyping and clarity
- Implement a **recursive descent parser (LL style)**
- Eventually produce valid assembly or binary output usable by our assembler


## References

- [ISO C99 Standard (N1256)](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf) — used as the language reference
- SBCL (Steel Bank Common Lisp) as the implementation language


## Directory Structure

```bash
compiler/
├── examples/
│   ├── return-39.c       # Minimal C program to test parsing: `int main(void) { return 39; }`
│   └── README.md           # Test program descriptions
├── lexer.lisp              # Tokenizes the input C code
├── parser.lisp             # (WIP) LL-style parser using recursive descent
├── codegen.lisp            # Will eventually emit IR or assembly
├── compiler.lisp           # Entry point for compilation
├── package.lisp            # Lisp package definition
├── mini-cc                 # Script to run the compiler
└── README.md               # This file
```

## Current Status

- Lexer and token stream implemented
- Parser under active development (LL-style recursive descent)
- Code generation not yet implemented (`codegen.lisp` is a stub)
- Testing minimal programs like `return-39.c`


## Example Input

**`examples/return-39.c`**
```c
int main(void) {
  return 39;
}
```

Used for testing tokenization and parsing of basic return statements.


## Running the Compiler
Currently, the entry point is `compiler.lisp`. Load it in the SBCL REPL:

```shell
$ sbcl --script compiler.lisp examples/return-39.c
```
or
```shell
$ ./mini-cc examples/return-39.c
```

## Future Goals
- Add AST to IR transformation
- Emit ARMv4 compatible assembly format
- Integrate with the `assembler/` and `linker/` stages
- Eventually self-host a tiny C program on our custom MCU