Heresy
======

Heresy is a BASIC-inspired functional dialect of Lisp, currently implemented as a Racket library.

Basic Lisp primitives are provided, including a basic DEF MACRO, as well as BASIC-like control structures and definition syntax. The intention is to provide a dialect of Lisp with a more familiar BASIC-inspired naming scheme, for those coming from that school. It also serves as a playground for experimenting with how to program without mutable variables, and exploring how traditional imperative control structures like loops might otherwise prove useful in such an environment.

This was primarily written as a learning exercise for practicing Racket macrology and thinking more functionally as a programmer. Future wishlist: more of the Basic and Lisp standard library, structs, pattern-matching/guards

The code Copyright 2014 by John S. Berry III, and is licensed via the LGPL v3.

Installation
------------

To install:

``raco pkg install git//github.com/jarcane/heresy``

To use, append this to the start of your file in DrRacket or your favorite text-editor:

``#lang s-exp heresy``

Quick Reference
---------------

### Declarations ###

```
(let ((*name* *value*) ...) *body* ...)
Defines local variables, to be used in subsequent code.

(def *name* *contents*)
(def fn *name* (*args* ...) *body* ...)
(def macro *name* (*pattern-vars* ...) *pattern* ...)
Defines new variables, functions, and macros

(fn (*args* ...) *body* ...)
The anonymous function
```

### Flow control ###

```
(if *test* then *do1* else *do2*)
(if *test* then *do1*)
Basic conditional execution block

(select [*test* *op1*] ... [else *opn*])
(select case *test* [*test-result* *op1*] ... [else *opn*])
Multiple conditional block: COND-style, or CASE style with CASE.

(for (*var* over *list*) *body* ... [carry *value*] [break [*value*]])
Iterates over list in val, CARRYing value assigned from accumulator to next loop
*cry* contains the accumulator, initialized to '()

(do *body* ...)
Executes its body in order (eqv. to begin)

(do loop *body* ... [break])
executes a block of code, looping continuously, until it encounters a BREAK
```

### I/O ###

```
(print *datum*)
Print datum with new line (akin to Racket displayln)

(print & *datum*)
Print datum without new line (akin to display)

(print lit *datum*)
Print literal form of datum such that it can be read by *input stx* (akin to write)

(input [*str*])
Accepts input as text and returns it (akin to read-line)
Adding a *str* causes it to output a text prompt first

(input stx)
Takes input as syntax and returns it (akin to read)
```

### Lists and Atoms ###

```
(join *a b*)
Joins two atoms into a pair, creating a list of second atom is another pair or null

(head *list*)
Returns the left item in a pair (the head of a list)

(tail *list*)
Returns the right item of a pair (the tail of a list)

(list? *list*)
Returns true (#t) if item is a list

(null? *list*)
Returns true if item is the empty list '()

(atom? *list*)
Returns true if item is an atom

(eq? *list*)
Returns true if atoms are the same

(lat? *list*)
Returns true if list is composed only of atoms (rather than other lists)

Null
Alternate alias for the empty list '()
```

### Math ###

```
(zero? *num*)
Returns true if zero

(= *a b* ...)
Returns true if all provided values are equal

(+ *a b* ...)
(- *a b* ...)
(* *a b* ...)
(/ *a b* ...)
(^ *a b*)
Math operators

(! *a fun b*)
Infix operator, allows *fun* to be used infix in this level only 
```

### Strings ###

```
(=$ *str* ...)
Compares strings for equality

(& *str* ...)
Concats strings

(list$ list)
Converts a string into a list of single character strings
```

### Logic ###

```
(and *a b* ...)
(or *a b* ...)
(not *a*)
Boolean operators

True, False
Alternate syntax for #t and #f
```

### Meta ###

```
(quote *datum*)
Returns datum as data instead of evaluating (can be shortened to ')

(run *form*)
Evaluates and executes the given form.

(rem ...)
Ignores its contents, evaluates to void
```
