# C Compiler


## The Structure of Compiler
There are two parts: *analysis* and *synthesis*

*analysis part (front end)*
- break up the source program into constituent pieces
- create an intermediate representation
- syntactically ill formed
- semantically unsound
- collects information about the source program
- stores the information in a data structure called a *symbol table*

*synthesis part (back end)*
- constructs the desired target program
- from intermediate representation and the informatino in the symbol table


## Phase of Compiler

Lexical Analyzer

Syntax Analyzer

Semantic Analyzer

Intermediate Code Generator

Machine-Independent Code Optimizer

Code Generator

Machine-Dependent Code Optimizer

(with Symbol Table)

## Phase 1: Lexical Analysis(Scanning)

## Phase 2: Syntax Analysis(Parsing)
Create a tree-like intermediate representation: syntax tree


## Phase 3: Semantic Analysis
An important part of semantic analysis is *type checking*.


## Phase 4: Intermediate Code Generator

## Phase 5: Code Optimization

## Phase 6: Code Generation
The code generator takes as input an intermediate representation of the source program and maps it into the target language.
The intermediate instructinos are translated into sequences of machine instructinos that perform the same task.
A crucial part is the judicious assignment of registers to hold variables.


---
# Which Parts of Compiler in Common?

## Lexer (Lexical Analyzer)
The first step is a lexical analysis from a ***Lexer***, in other words *Scanner*.  
The main job is to make tokens from a stream of characters. For example,
```c
int a = 3;
```
- int
- a
- =
- 3
- ;

So, the main thing that I have to think about is **how can I generate tokens from a flat character sequence**.

Moreover, all tokens have a meaningful information, like a it's type.
- Token Type
  - keywords
    - int, float, char
    - return
    - if, else if, else
    - while, do, for
    - define
  - one-character
    - =
    - +, -, *, /, %
    - <, >
    - \#
    - (, )
    - {, }
    - ;
  - two or more character
    - ==, >=, <=
    - <<, >>
    - +=, -=, *=, /=, %=
  - literals
    - number
    - string
    - identifier (variable name, function name)


## Parser (Syntax Analyzer)
The next step is *parsing*.
Commonly, produce a ***parse tree or syntax tree***

## Semantic Analyzer (Type Checking, Static Analysis)
Chekcs variable scope, type error, make a symbol table.

## Intermediate representation


## Code Generator (Assembly Generator)



## Single-Pass Compiler
- No IR(Intermediate Representation) or a syntax tree.
- Pascal and C


## References
- **Open Source C Compiler**
  - [chibicc](https://github.com/rui314/chibicc)
  - [ucc](https://github.com/sheisc/ucc162.3)
- **Books & Papers**
  - C Language Standard
    - [ANSI C](https://en.wikipedia.org/wiki/ANSI_C)
      - [C99](https://www.dii.uchile.cl/~daespino/files/Iso_C_1999_definition.pdf)
    - [The syntax of C in EBNF](https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm)
  - Compiler Design
    - [An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf)
    - [Crafting Interpreters](https://craftinginterpreters.com/contents.html)
    - Compilers - Principles,Techniques, and Tools(Dragon Book)
    - [ANSI C Yacc grammer](https://www.quut.com/c/ANSI-C-grammar-y.html)
  - Lisp
    - SICP
    - [Practical Common Lisp](https://gigamonkeys.com/book/)
    - [Common Lisp the language, 2nd Edition](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/clm.html)
    - [Lisp in Small Pars](http://lisp.plasticki.com/)