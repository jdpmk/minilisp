# `minilisp`

`minilisp` is a minimal dialect of [LISP](https://en.wikipedia.org/wiki/Lisp_(programming_language)), implemented in [Haskell](https://www.haskell.org/), with specifications derived from the [LISP 1.5 Programmer's Manual](http://www.softwarepreservation.org/projects/LISP/book/LISP%201.5%20Programmers%20Manual.pdf) published by [John McCarthy](https://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)) in 1962.

## Setup

Prerequisites:
- [Glasgow Haskell Compiler (GHC)](https://www.haskell.org/ghc/) (with GHCi REPL)

```sh
$ git clone https://github.com/jdpmk/minilisp
$ cd minilisp
$ ghc -o minilisp Main.hs
$ ./minilisp
```

## Features

- [X] Lisp Primitives (`cons`, `car`, `cdr`, `eq`, `atom`, `quote`, etc.)
- [X] Anonymous function evaluation
- [ ] Function definition and evaluation within a context
- [ ] Arithmetic operations
- [ ] Better error messages

## Examples

```lisp
λ >> (cons 'a 'b)
(a b)
λ >> (car (cons 'a 'b))
a
λ >> (cdr '(a b c))
(b (c nil))
λ >> ((lambda (x) x) 'a)
a
λ >> ((lambda (x y) (eq x y)) 'a 'b)
nil
λ >> ((lambda (x y) (eq x y)) 'a 'a)
t
λ >> ((lambda (x y) (cond ((eq x y) "yes") (t "no"))) 'a 'b)
"no"
λ >> ((lambda (x y) (cond ((eq x y) "yes") (t "no"))) 'a 'a)
"yes"
```

