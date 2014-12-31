#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label heresy
                     (only-in racket/base require)))
@(define (make-heresy-eval)
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'heresy)))
@(define-syntax-rule @myexamples[stuff ...]
   @examples[#:eval (make-heresy-eval) stuff ...])
@(define-syntax-rule @mytabular[[cell ...] ...]
   @tabular[#:sep @hspace[1] (list (list cell ...) ...)])

@title{The Heresy Programming Language}

@author{John S. Berry III}

source code: @url["https://github.com/jarcane/heresy"]

@(hash-lang) @racketmodname[s-exp] @racketmodname[heresy]
@defmodule[heresy]

The Heresy language is a functional Lisp/Scheme dialect implemented in Racket,
with syntax inspired by the BASIC family of programming languages. Its principle
goals are to provide a simple core language for BASIC and other programmers to
experiment with and learn how to program functionally. This document will detail
the general philosophy of the Heresy language, such as exists, as well as the
language syntax and functions. 

The Heresy language was created by John S. Berry III with additional contributions
from many others in the Racket community.

Heresy and this documentation are Copyright (c) 2014 John S. Berry III and
released under the terms of the GNU LGPL.

@section{The Heresy Rules}

The Heresy language is developed according to a few basic "ground rules," which
the author and contributors attempt to follow in developing new features and
functions for the language. These are as follows:

@itemlist[
  #:style 'ordered
  @item{@bold{Heresy is BASIC} - Heresy is inspired by BASIC, and aims to be at
        least somewhat easy for BASIC programmers to learn. Mostly this means we
        prefer BASIC names for functions over the Lisp name, and naming
        conventions like the $ for string functions.}
  @item{@bold{Heresy is a Lisp} - Heresy is still a Lisp, and loves simple syntax
        and s-expressions. While it makes use of some sugaring like literal
        keywords for certain common primitives, these are best used sparingly.
        Heresy is the Diet Coke of Evil, just one calorie, not quite evil enough.}
  @item{@bold{Heresy is functional} - Functional, but not Haskell. It is not
        intended solely as a vehicle for absolute functional purity. I love
        Haskell. You love Haskell. We don’t need to  write another Haskell. Think
        more in terms of a lower-calorie, more intelligible Clojure.}
  @item{@bold{Heresy is for learning} - Heresy started as a learning project, a
        chance to learn how Lisp and functional programming really work on a
        practical level. I hope that, in time, it can be that for others as well,
        especially those who grew up with BASIC like myself and still sometimes
        struggle to get their head around the functional style. In particular,
        this means the Heresy-written portions of the source are generally
        written in as clear a manner as possible, as they are intended to be
        self-teaching.}
  @item{@bold{Heresy is an experiment} - Heresy is an experimental language.
        It’s very DNA is as a mad idea that came to life, and it’s development
        should be ready and willing to embrace new mad ideas and run with them.
        This is where carry came from, and I hope to have more mad ideas in the
        future.}
  @item{@bold{Heresy is for everyone} - As a statement of culture, the Heresy
        community welcomes the contribution of all people, who taste equally
        delicious to the jaws of mighty Cthulhu. No discrimination, harassment,
        or any other mistreatment of contributors on the basis of age, race,
        sexuality, or gender will @bold{ever} be tolerated by myself or anyone
        else who wishes to be part of this project.}
]

@section{Heresy Syntax and Conventions}

Generally speaking, Heresy follows standard s-expression syntax as expected from
any Lisp, being composed of parenthesized sequences of terms in Polish notation.
Each sequence thus begins with an operator or function, and any number of
arguments or additional s-expressions as needed.

There are however a few exceptions to usual expectations in the case of certain
special forms like @racket[for], @racket[def], and @racket[if]. These make use
of additional literal terms as part of their syntax, to provide more clarity and
similarity to BASIC-style syntax.

In accordance with that goal, Heresy also follows certain naming conventions as
a matter of style. Functions which produce a string value are appended with $,
and in general where a naming conflict between two similar functions in
Racket/Scheme and BASIC exists, prefer BASIC.

When borrowing BASIC syntax and naming for use in Heresy, the author has
generally relied chiefly on QBASIC and ECMA BASIC for reference.

@section{Heresy Reference}

@subsection{Declarations}

@defform[(def name value)]{
Defines a new variable of @racket[name] with the given @racket[value].
}

@defform[#:link-target? #f #:literals (fn)
         (def fn name args body ...+)
         #:grammar ([args (arg ...) (arg ... . rest-id) args-id]
                    [arg arg-id [arg-id default-expr]])]{
Defines a function of @racket[name], which when called evaluates its body
expressions with the given list of arguments bound to local variables for use in
the body of the function’s definition. Note that there are a number of
additional options here for defining arguments. Default values can be ascribed
to an argument by enclosing it in additional parentheses:
@myexamples[
  (def fn foo (x (y 1)) (+ x y))
  (foo 3 4)
  (foo 5)
]
Two patterns as well exist for taking an arbitrary number of values. The
argument names list can be forgone entirely and substituted with a single name
(generally args* by convention), which will then contain a list of any and all
values passed to the function. The second method is the use of the dot (.) in
the body of the arguments list followed by a single variable (usually called rest).
@myexamples[
  (def fn foo args* args*)
  (foo)
  (foo 3 4 5)
  (def fn bar (x y . rest) (join (+ x y) rest))
  (bar 3 4)
  (bar 3 4 5 6 7 8)
]}

@defform[(let ((name value) ...) body ...+)]{
Binds the given name-value pairs for use in the local context created by the
body of the expression. This is used to define local variables, such as are
needed within a function. Note that local functions can potentially be assigned
this way by storing anonymous functions, but there is a built-in syntax for
defining a single such function, like so:
@defform[#:link-target? #f (let proc-id ((name value) ...) body ...)]{
When let is called this way, it defines a local function proc (conventionally
called recur), which can then be called from within the body of the let in order
to perform local recursion; the name-value pairs thus act as arguments to the
function proc.
}}

@defform[(fn (arg ...) body ...)]{
Creates an anonymous function with the given arguments, that evaluates its body
when called. This is the lambda expression from other Lisps and functional
languages, and a given fn can be passed as a value (as can named functions, for
that matter) wherever called for. An anonymous function can also be evaluated
directly in place by using it as the operator in an expression, like so:
@myexamples[
  ((fn (x y) (* x y)) 4 5)
]}

@subsection{Conditionals and Loops}

@defform[#:literals (then else)
         (if test then texpr else fexpr)]{
Evalutes @racket[test] and, if @racket[test] is @racket[True], evaluates
@racket[texpr], otherwise it evaluates @racket[fexpr]. Note that only a single
expression can be placed in each "slot" in the syntax; if you need to do
multiple things, use a @racket[do] block.
}

@defform*[#:literals (else)
          [(select (test1 expr1) ...)
           (select (test1 expr1) ... (else fexpr))]]{
Given a list of test-expression pairs, evaluates the tests in order until it
finds one which is @racket[True], and evaluates the matching expression. The
@racket[else] expression is always true: if an else is found at the end of the
select statement, its matching @racket[fexpr] will be evaluated. If no test in
select is true, returns @|void-const|. 
}

@defform*[#:link-target? #f #:literals (case else)
          [(select case texpr ((val ...) rexpr) ...)
           (select case texpr ((val ...) rexpr) ... (else fexpr))]]{
Evaluates @racket[texpr] and compares it to each @racket[val] in turn until it
finds a value that is @racket[eq?] to the result of @racket[texpr]. If one is
found, it evaluates the matching @racket[rexpr]. Like with @racket[select],
@racket[else] is always considered True, and will therefore always evaluate its
@racket[fexpr]. If no matching @racket[val] is found, @racket[select case]
evaluates to @|void-const|. Note also that the @racket[(val ...)] is a list, and
can contain as many values as is needed, such as in the following example:
@myexamples[
  (select case (* 2 3)
    ((2 3 4) (print "Nope."))
    ((5 6 7) (print "Yup."))
    (else (print "something is horribly wrong.")))
]}

@defform*[#:literals (in with)
          [(for (var in list) body ...)
           (for (var in list with cry) body ...)]]{
Iterates over list evaluating its body with the head of list assigned to var,
then recurs with the tail of list until it returns @racket[Null]. @racket[for]
loops declare an implicit variable @racket[cry] which can be passed a value with
@racket[carry]. They may also be interrupted with @racket[break]. See below for
more details.
}

@defform[(do body ...)]{
Evaluates its @racket[body]s in order, returning the result of the final body
expression.
}

@defform*[#:link-target? #f #:literals (loop with)
          [(do loop body ...)
           (do loop with cry body ...)]]{
Evaluates body repeatedly until a @racket[break] statement is encountered.
Declares the implicit variable @racket[cry], which can be reassigned with the
@racket[carry] operator.
}

@defform*[[(break)
           (break value)]]{
Breaks the continuation of a @racket[for] or @racket[do] loop evaluation. If
provided a value, returns that value as the result of the loop.
}

@defform[(carry value)]{
When called in the body of a @racket[for] or @racket[do] loop expression,
immediately begins the next iteration of the loop, and passes the given value to
the implicit variable @racket[cry].
}

@defidform[cry]{
Loops declare an internal variable called @racket[cry], which defaults to
@racket[Null], and which is passed automatically to the next iteration of the
loop, and is returned when the loop concludes. The value of @racket[cry] can be
specified at the beginning of the loop with the optional @racket[with]
parameter, and @racket[carry] can be used to pass a new value of @racket[cry] to
the next iteration.
}

@subsection{Predicates and Logic}

@defproc[(list? [v any]) boolean?]{
Returns True if @racket[v] is a list.
}

@defproc[(null? [v any]) boolean?]{
Returns True if @racket[v] is @racket[Null], where Null is defined as the empty list @racket['()].
}

@defproc[(zero? [v any]) boolean?]{
Returns True if @racket[v] = 0.
}

@defproc[(one? [v any]) boolean?]{
Returns True if @racket[v] = 1.
}

@defproc[(eq? [x any] [y any]) boolean?]{
Returns True if x and y are the same object.
}

@defproc[(equal? [x any] [y any]) boolean?]{
Returns True if x and y are equal.
}

@defproc[(symbol? [v any]) boolean?]{
Returns True if @racket[v] is a symbol: ie. a quoted name such as @racket['foo].
See @racket[quote] in @secref["syntax-and-evaluation"].
}

@defproc[(atom? [v any]) boolean?]{
Returns True if @racket[v] is an atom: ie. a number, symbol, or procedure,
rather than a list or Null.
}

@defproc[(lat? [l any]) boolean?]{
Returns True if @racket[l] is a list composed solely of atoms.
}

@defform[(and expr ...)]{
Returns True only if all given expressions are True.
}

@defform[(or expr ...)]{
Returns True if any given expression is True.
}

@defproc[(not [v any]) boolean?]{
Returns True if v is False, else returns False.
}

@defidform[else]{
A special keyword for True, used as a literal in conditional statements.
}

@defthing[True boolean?]{
The boolean truth value. Actually an alias for @racket[#t] in the Racket
implementation. Note that, in Heresy, as in Racket, anything not explicitly
False is considered True.
}

@defthing[False boolean?]{
The boolean false value. Actually an alias for @racket[#f] in the Racket
implementation.
}

@defthing[Null null?]{
An alias for the empty list @racket['()].
}

@subsection[#:tag "syntax-and-evaluation"]{Syntax and Evaluation}

@defform*[[(quote v)
           @#,code{'v}]]{
"Quotes" the given @racket[v], without evaluating its contents. A quoted list is
passed merely as data, a quoted atom is a "symbol" as per @racket[symbol?]. Can
be shortened to @litchar{'}.
}

@defform*[[(quasiquote v)
           @#,code{`v}]]{
Same as @racket[quote], but can be "escaped" with the @racket[unquote] and
@racket[unquote-splicing] syntax. Can be shortened to @litchar{`}.
}

@defform*[[(unquote v)
           @#,code{,v}]]{
When encountered within a a @racket[quasiquote]d block, evaluates @racket[v] and
quotes its value instead. Can be shortened to @litchar{,}.
}

@defform*[[(unquote-splicing v)
           @#,code|{,@v}|]]{
Similar to @racket[unquote], but splices the result of evaluating @racket[v] in
place. Can be shortened to @litchar|{,@}|.
}

@defproc*[([(error [message string?]) nothing]
           [(error [symbol symbol?] [message string?]) nothing])]{
Halts the program, returning an error of @italic{@tt{symbol: message}} where
@racket[symbol] is a quoted value (customarily the name of the current function)
and @racket[message] is a string.
}

@defproc[(run [form any]) any]{
Evaluates the given form. Usage is not recommended.
}

@defform[(rem any ...)]{
Ignores its arguments and returns void. Useful for block comments.
}

@defform[#:link-target? #f #:literals (macro)
         (def macro name (pattern ...) template)]{
Defines a new macro with @racket[name]. A macro can best be thought of as a
function which is not evaluated, but rather returns syntax to be evaluated in
the form of a template. Each name described in the @racket[pattern] defines a
"pattern variable" which can then be used in the body of the @racket[template]
and will pass any syntax contained in that portion of the @racket[pattern] in
the appropriate location matched in the @racket[template]. The elipsis
@racket[...] can be used in a pattern to indicate repeatable values.
}

@defproc[(apply [fun procedure?] [v any] ... [lst list?]) any]{
Applies @racket[fun] to the given arguments, as if it had been called with
@racket[(fun v ... x ...)] where the @racket[x]s are the elements in @racket[lst].
}

@subsection{Input and Output}

@defform*[#:literals (& lit)
          [(print v)
           (print & v)
           (print lit v)
           (print)]]{
Prints the given @racket[v] to the current output, or stdout if not otherwise
specified, followed by a newline. @racket[(print & v)] outputs without a
newline, while @racket[(print lit v)] outputs as a literal form that can be
directly read back by @racket[(input stx ....)] as code. A bare @racket[(print)]
merely prints a newline to the current output.
}

@defform[(? ....)]{
A shortened macro for print.
}

@defform*[#:literals (stx)
          [(input)
           (input stx)
           (input string)
           (input stx string)]]{
Reads a line of input from the current input, or stdin if not otherwise
specified, and returns the value read as a string. @racket[(input stx ....)]
instead reads a value using the standard reader, thus providing syntax which can
be evaluated with @racket[run]. If additionally provided with a string, this
will be output as a prompt to the current output.
}

@defform[(using io-port body ...)]{
Evaluates the body, with input/ouptut redirected to the given io-port. Only the
@racket[file] port is supported at this time.
}

@defform[#:literals (as)
         (file name as mode)]{
Opens the file @racket[name] as the new target for input or output, depending on
the @racket[mode] provided. @racket[mode] is a symbol, of one of the following:
@mytabular[
[@racket['output]  @elem{Opens file as the current output port. Will fail if
                         file already exists.}]
[@racket['rewrite] @elem{Opens file as the current output port, rewriting its
                         contents if the file exists.}]
[@racket['input]   @elem{Opens file as the current input port.}]]
}

@subsection{Lists}

@defproc[(list [v any] ...) list?]{
Returns a list containing the given values.
}

@defproc[(join [a any] [b any]) pair?]{
Joins @racket[a] and @racket[b] into a pair. If @racket[b] is @racket[Null], a
list is created containing @racket[a]. If @racket[b] is a list, @racket[a] is
joined to the head of the list.
}

@defproc[(head [l list?]) any]{
Returns the head (first element) of the list @racket[l].
}

@defproc[(tail [l list?]) any]{
Returns the remainder of list @racket[l] after the head. If the list has only
one element, returns @racket[Null].
}

@defform*[#:literals (to step)
          [(range start to finish)
           (range start to finish step n)]]{
Generates a list of numbers, incrementing from @racket[start] to @racket[finish]
by @racket[n]. If no @racket[n] is provided, defaults to 1. Note that, unlike
BASIC’s @tt{for x = y to z}, descending sequences where @racket[start] is
greater than @racket[finish] can only be declared by including a negative n.
Otherwise only @racket[Null] will be returned.
}

@defproc[(map [fun procedure?] [l list?]) list?]{
Given a single-argument function @racket[fun], returns a list with @racket[fun]
applied to each item in @racket[l].
}

@defproc[(filter [fun procedure?] [l list?]) list?]{
Given a predicate @racket[fun], returns a new list containing only those
elements of @racket[l] for which @racket[fun] returns True.
}

@defproc[(len [l list?]) number?]{
Returns the number of items in @racket[l].
}

@defproc[(foldr [fun procedure?] [base any] [l list?]) any]{
Given a function @racket[fun] with two arguments, returns the cumulative result
of @racket[fun] being applied to consecutive pairs, starting from @racket[base]
and the rightmost element of @racket[l].
}

@defproc[(foldl [fun procedure?] [base any] [l list?]) any]{
Similar to @racket[foldr], except that it combines pairs from the left, starting
with the head of @racket[l] and @racket[base].
}

@defproc[(reverse [l list?]) list?]{
Returns a list with the order of @racket[l] reversed.
}

@defproc[(index [n number?] [l list?]) any]{
Returns the nth entry of l, indexed from 1.
}

@defproc[(index* [l list?] [dims number?] ...) any]{
Walks through nested lists according to the given @racket[dims], essentially
finding index recursively for an arbitrary number of dimensions. For example,
given a nested list three lists deep, @racket[(index* l 2 3 1)] would return the
1st element of the third element of the 2nd lst, like so:
@myexamples[
  (def dave '(1 (2 3 (4 5)) 6))
  (index* dave 2 3 1)
]}

@defproc[(inlst [item any] [l list?]) any]{
Searches @racket[l] for @racket[item], returning the index of @racket[item] if
found, or @racket[False] if not.
}

@defproc[(left [l list?] [n number?]) list?]{
Returns a list of the leftmost @racket[n] elements of @racket[l].
}

@defproc[(right [l list?] [n number?]) list?]{
Returns a list of the rightmost @racket[n] elements of @racket[l].
}

@defproc[(mid [l list?] [idx number?] [n number?]) list?]{
Returns @racket[n] entries of @racket[l] starting from index @racket[idx].
}

@defproc[(slice [l list?] [first number?] [last number? (len l)]) list?]{
Returns a slice of @racket[l], starting at @racket[first] and ending at
@racket[last]. If @racket[last] is not provided, it defaults to the end of the
list.
}

@defproc[(append1 [l1 list?] [l2 list?]) list?]{
Returns a list with @racket[l2] appended to the end of @racket[l1].
}

@defproc[(append [l list?] ...) list?]{
Returns a list of the given @racket[l]s appended together in order.
}

@defproc[(assoc [tgt any] [l list?]) list-or-false?]{
Searches the heads of a list of lists @racket[l] and returns the first matching
list or @racket[False].
}

@defproc[(subst [tgt any] [new any] [l list?]) list-or-false?]{
Searches the heads of a list of lists @racket[l], and if it finds @racket[tgt],
returns a new list with the tail of tgt substituted for @racket[new]. Otherwise,
returns @racket[False].
}

@defproc[(heads [l list?]) list?]{
Returns a list of the heads of a list of lists.
}

@defproc[(sort [fun procedure?] [l list?]) list?]{
Sorts list @racket[l] according to the comparison function @racket[fun].
}

@defproc[(zip [l1 list?] [l2 list?]) list?]{
Returns a new list of lists combining @racket[l1] and @racket[l2]. Excess length
of either list is dropped.
}

@defproc[(zipwith [fun procedure?] [l1 list?] [l2 list?]) list?]{
Returns a new list, combining the matching pairs of each list with @racket[fun].
Excess length of either list is dropped.
}

@subsection{Strings}

@defproc[(=$ [x string?] [y string?]) boolean?]{
Returns True if the two strings are equal.
}

@defproc[(& [str string?] ...) string?]{
Concatenates its arguments into a single string.
}

@defproc[(list$ [str string?]) string?]{
Returns a list of one-character strings from the given string.
}

@defproc[(str$ [n number?]) string?]{
Converts a number @racket[n] to a string.
}

@defproc[(empty$? [str string?]) boolean?]{
Returns True if the string is empty (@racket[""]).
}

@defproc[(len$ [str string?]) number?]{
Returns the length of the string, indexed from 1.
}

@defproc[(list& [l list?]) string?]{
Given a list of strings, returns a single concatenated string.
}

@defproc[(head$ [str string?]) string?]{
Returns the head (first character) of the string.
}

@defproc[(tail$ [str string?]) string?]{
Returns the tail (remaining characters) of the string, unless @racket[str] is
empty, in which case it returns the empty string.
}

@defproc[(left$ [str string?] [n number?]) string?]{
Returns a string of the leftmost @racket[n] characters of @racket[str].
}

@defproc[(right$ [str string?] [n number?]) string?]{
Returns a string of the rightmost @racket[n] characters of @racket[str].
}

@defproc[(mid$ [str string?] [idx number?] [len number?]) string?]{
Returns a section of @racket[str], @racket[len] characters long, beginning at
@racket[idx].
}

@defproc[(slice$ [str string?] [start number?] [finish number? (len$ str)]) string?]{
Returns a slice of @racket[str] beginning at @racket[start] and ending at
@racket[finish]. If not specified, @racket[finish] defaults to the length of the
string.
}

@defproc[(instr [str string?] [search string?]) number-or-false?]{
Returns the index of the first instance of @racket[search] in @racket[str], or
False if not found.
}

@defproc[(split [str string?] [delimiters list? '(" ")]) list?]{
Returns a list of string sections split at the given delimiters. If
@racket[delimiters] is not specified, defaults to space (@racket[" "]).
}

@subsection{Math}

@defproc[(+ [x number?] ...) number?]{
Adds the given numbers left to right and returns the result. If only one argument is given, returns the argument. If no arguments are provided, returns 0.
}

@defproc*[([(- [x number?] [y nuber?] ...+) number?]
           [(- [x number?]) number?])]{
Subtracts the given numbers left to right and returns the result. If only one
argument is given, returns @racket[(- 0 x)].
}

@defproc*[([(/ [x number?] [y number?] ...+) number?]
           [(/ [x number?]) number?])]{
Divides the numbers from left to right and returns the result. If only one
argument is given, returns @racket[(/ 1 x)].
}

@defproc[(* [x number?] ...) number?]{
Multiplies the numbers given from left to right and returns the result. If no
argument is given, returns one. If one argument is given, returns the argument.
}

@defproc[(= [x number?] [y number?] ...) boolean?]{
Returns True if all the numbers are numerically equal.
}

@defproc[(< [x number?] [y number?] ...) boolean?]{
Returns True if all arguments are greater than the one previous going right
(ie, x < y < z, etc.)
}

@defproc[(> [x number?] [y number?] ...) boolean?]{
Returns True if all arguments are less than the one previous going right
(ie, x > y > z, etc.)
}

@defthing[pi number?]{
A bound variable containing the 64-bit floating point value of pi.
}

@defthing[e number?]{
A bound variable containing the 64-bit floating point value of Euler’s number.
}

@defproc[(mod [x number?] [y number?]) number?]{
Returns the modulus of @racket[x] divided by @racket[y].
}

@defproc[(abs [n number?]) number?]{
Returns the absolute value of @racket[n].
}

@defproc[(even? [n number?]) boolean?]{
Returns True if @racket[n] is even.
}

@defproc[(odd? [n number?]) boolean?]{
Returns True if n is odd.
}

@defproc[(sgn [n number?]) number?]{
Returns @racket[-1] if @racket[n] is negative, @racket[0] if @racket[n] is zero,
and @racket[1] if @racket[n] is positive.
}

@defproc[(inc [n number?]) number?]{
Returns the value of @racket[(+ n 1)].
}

@defproc[(dec [n number?]) number?]{
Returns the value of @racket[(- n 1)].
}

@defproc[(exp [x number?]) number?]{
Returns the value of @elem{@racket[e]@superscript{@racket[x]}}.
}

@defproc[(sin [x number?]) number?]{
Returns the sine of @racket[x] as a floating point value.
}

@defproc[(cos [x number?]) number?]{
Returns the cosine of @racket[x] as a floating point value.
}

@defproc[(tan [x number?]) number?]{
Returns the tangent of @racket[x] as a floating point value.
}

@subsection{Random Numbers}

Heresy’s random number generator operates slightly differently to traditional
BASIC's, in order to offer a more functional approach. Rather than defining a
single global seed which the RND function then refers to, Heresy's
@racket[randomize] returns a "generator" function with a given seed, allowing
one to name and seed as many generators as one needs, though for practical
purposes a default RND is still provided which is automatically created and
seeded with a derivation of the current time in milliseconds.

@defproc[(randomize [seed number? timer]) procedure?]{
Returns a new generator function initialized with @racket[seed]. If no
@racket[seed] is provided, defaults to @racket[timer].
}

@defproc[(rnd) number?]{
A pre-defined generator which returns a random number between @racket[0] and
@racket[1], exclusive, seeded by @racket[timer].
}

@defthing[timer number?]{
A special internal variable which contains the current time in milliseconds.
}

@subsection{Things}

Things are Heresy's definable data structures. Unlike the objects of most
object-oriented languages, which often exist to hold and carry mutable state and
actions with which to change that state, Things are immutable. A Thing, once
sprung to life, cannot itself be altered, but can be called with the correct
syntax to return a new Thing with different internal values for its internal
data fields.

Things are essentially functions, lambdas specifically, with predefined syntax
arguments. They are first class values, and can be passed freely just as any
other data, but can also be passed arguments to either return the values of
their fields, return a new Thing, or to employ any functions contained within
the thing.

@defform[(describe Name (field value) ...)]{
Defines a new type of Thing, given @racket[Name]. By convention, Things are
generally named in uppercase, though this is not required by the syntax. Each
field is an internal name and external symbol, which is mapped to the given
value. Anonymous functions (@racket[fn]) can be assigned as values to Thing
fields, and those functions can access the fields of the Thing by name.
}

If there is a Thing defined as @defidentifier[#'Name]:
@defform*[#:kind "" #:link-target? #f
          [(Name)
           (Name symbol)
           (Name @#,racket['fields])
           (Name pattern)]]{
Once a Thing has been described or bound to a name by other means, that Name is
bound locally as a function, and can thus be called with special syntax to
return its contents or to return a new copied Thing. In more detail, these
syntaxes are as follows:

@defform[#:kind "" #:link-target? #f (Name)]{
Returns an association list containing the contents of the Thing, ie. a list in
the form of: @racket['((field value) ...)]
}

@defform[#:kind "" #:link-target? #f (Name @#,racket['fields])]{
Returns a list of symbols for the fields contained within the Thing. Note that
the symbol @racket['fields] takes precedent over the field names within, in
order to prevent overwriting this syntax.
}

@defform[#:kind "" #:link-target? #f (Name symbol)]{
Returns the value of the field associated with @racket[symbol], the quoted
form of the field name described when the Thing type was first declared. Will
return an error if no such named field is found. If the value associated with
symbol is a function, this expression can be used as the operating function of
a further expression like so:
@myexamples[
  (describe Lord-Cthulhu (eat (fn (x) (print (& "Devours " x)))))
  ((Lord-Cthulhu 'eat) "Dave")
]}

@defform[#:kind "" #:link-target? #f (Name pattern)
                #:grammar ([pattern @#,racket[`(@#,racketvarfont{sub-pat} ...)]]
                           [sub-pat * value])]{
Returns a copy of the Thing, with new values according to the pattern passed to
the original Thing. @racket[pattern] must be a quoted list of either
@racket['*]s or values, in order according to the fields of the Thing as
originally defined (so the first @racket[sub-pat] matches the first
field, the second to the second field, and so on). A @racket['*] in a field
indicates that the value is copied in-tact, while a value becomes the new value
of the field in that position. For example:
@myexamples[
  (describe Santa
            (size 'fat)
            (sleigh 'ready)
            (sack 'full))
  (def Santa-after-Christmas (Santa `(* * empty)))
  (Santa-after-Christmas)
]}}

@defproc[(send [Thing thing?] [symbol symbol?] [arg any] ...) any]{
An alternate syntax for accessing functions within Things, send calls the
function named by @racket[(Thing symbol)] with the given arguments and returns
the result.
}

@defform[(Self ....)]{
@racket[Self] is the self-referring identifier for a Thing, allowing for
functions within Things to call the Thing itself. Note that if it is only the
values of the other fields, this is not necessary, as fields are defined as
local names within the context of the Thing, and thus can be referred to simply
by name.
}

@subsection{Theory}

@defproc[(Y [fn procedure?]) procedure?]{
The strict Y fixed-point combinator. Allows for recursion of anonymous
functions. Given a @racket[fn1] which contains a single named argument, and
within which is an additional single-argument @racket[fn2], the innermost
@racket[fn2] can call the named argument of @racket[fn1] as if it were a
function name in order to recur on itself. For example, the factorial function
can be defined thusly, using the Y-combinator:
@myexamples[
  (def Fact   
    (Y     
     (fn (fact)       
       (fn (n)         
         (if (zero? n)            
             then 1             
             else (* n (fact (- n 1))))))))
]
Note however that usage of the Y-combinator for recursion is not especially
efficient, and the more traditional recursive approach is generally recommended
whenever possible (which is most of the time).
}

@defproc[(partial [fun procedure?] [arg any] ...) procedure?]{
Returns a function with the @racket[arg]s partially applied to @racket[fun],
which can then be passed the remaining arguments, as many as needed to complete
the calculation. For example:
@myexamples[
  (map (partial + 2) (range 1 to 4))
]}

@defproc[(compose [fn1 procedure?] [fn2 procedure?]) procedure?]{
Returns a new function which is a composition of @racket[fn1] and @racket[fn2].
This function evaluates @racket[fn2] with its arguments, and then applies
@racket[fn1] to the result of @racket[fn2].
@myexamples[
  (def abs-sub (compose abs -))
  (abs-sub 4 5)
]}

