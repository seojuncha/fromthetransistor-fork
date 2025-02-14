# C Compiler

# Implementation
1. Read a C source file path from a command-line argument.
2. Read line by line from the source file, then print each line.


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



# References
- **Open Source C Compiler**
  - [chibicc](https://github.com/rui314/chibicc)
  - [ucc](https://github.com/sheisc/ucc162.3)
- **Books & Papers**
  - Compiler Design
    - http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf
    - [Crafting Interpreters](https://craftinginterpreters.com/contents.html)
    - Dragon Book
  - Lisp
    - SICP
    - [Practical Common Lisp](https://gigamonkeys.com/book/)
    - [Common Lisp the language, 2nd Edition](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/clm.html)
    - [Lisp in Small Pars](http://lisp.plasticki.com/)