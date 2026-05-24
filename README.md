# Composable Language Parsers

An experimental parser-combinator library written in Gerbil Scheme, with the long-term goal of serving as the front-end for a compiler targeting the COOL language from Alex Aiken’s classic compiler course.

This project is a personal hobby project that I work on during weekends, vacations, and spare pockets of free time as a way to combine two long-standing interests: compilers and Lisp systems.

There’s something uniquely satisfying about feeding syntax into a parser and watching it emerge as a structured Lisp tree. This repository is a playground for exploring parser combinators, recursive grammars, AST generation, and language tooling in a highly composable functional style.

The project is still heavily experimental and evolving.

---

# Design Goals

A major goal of this project — inspired heavily by the ideas in Software Design for Flexibility — is to explore the expressive power of parser combinators as a style of programming, not just as a parsing technique.

Rather than relying heavily on dense parsing logic or large regular expressions, the goal is to construct grammars out of smaller reusable parser units that can be combined into larger structures declaratively.

The parser definitions are intended to communicate the *shape* of the grammar directly in code.

For example, binary-expression parsing is assembled by composing smaller parser components together: 

```scheme id="a8f3mx"
(def (parse-binary-exp operators (greedy #t) (followed-by (peek-empty)))
  (let* ((parser-builder
          (lambda ()
            (parse-pipeline [
                             (parse-expression)
                             (parse-any-char operators)
                             (parse-expression greedy followed-by)])))
         
         ...)
    
    (make-parser "parse-binary-exp"
                 parser-builder
                 ...)))
```

More complex grammar structures can then be built from the same reusable parser abstractions: 

```scheme id="d2v7pk"
(parse-pipeline
 [
  (parse-keyword "If")
  (parse-expression-followed-by (peek-keyword "Then"))
  (parse-keyword "Then")
  (parse-expression-followed-by (peek-keyword "Else"))
  (parse-keyword "Else")
  (parse-expression-followed-by (peek-keyword "Fi"))
  (parse-keyword "Fi")
 ])
```

The broader objective is to make parser definitions modular, composable, and expressive enough that the grammar structure remains visible directly in the code itself.

---

# Future Directions

* Improved AST-building abstractions
* Better error reporting
* More ergonomic parser combinators
* Grammar visualization tools
* Macro-assisted grammar definitions
* Additional COOL language coverage
* Eventually lowering into a larger compiler pipeline

---

# Inspiration

* Structure and Interpretation of Computer Programs
* Software Design for Flexibility
* Functional parser combinator systems
* Lisp metaprogramming
* Compiler and interpreter design
