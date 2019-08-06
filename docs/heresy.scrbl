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

@title[#:style '(toc)]{The Heresy Programming Language}

@author{Annaia Berry}

source code: @url["https://github.com/jarcane/heresy"]

@defmodule[heresy #:lang]

The Heresy language is a functional Lisp/Scheme dialect implemented in Racket,
with syntax inspired by the BASIC family of programming languages. Its principle
goals are to provide a simple core language for BASIC and other programmers to
experiment with and learn how to program functionally. This document will detail
the general philosophy of the Heresy language, such as exists, as well as the
language syntax and functions. 

The Heresy language was created by Annaia Berry with additional contributions
from many others in the Racket community.

Heresy and this documentation are Copyright (c) 2014 Annaia Berry and
released under the terms of the GNU LGPL.

@table-of-contents[]

@section[#:tag "rules"]{The Heresy Rules}

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

@section[#:tag "syntax"]{Heresy Syntax and Conventions}

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

@section[#:tag "reference" #:style '(toc)]{Heresy Reference}

The following sections describe the forms and functions of the Heresy programming language
in more detail, subdivided by category for easier navigation.

@local-table-of-contents[]

@subsection[#:tag "declarations"]{Declarations}

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

@defform*[#:link-target? #f #:literals (macroset)
          ((def macroset name [(name pattern ...) template] ...)
           (def macroset name (literal ...) [(name pattern ...) template] ...))]{
 Similar to @racket[def macro], except that multiple matching patterns can be defined
 allowing for macros with variable syntax. Like @racket[def macro], the @racket[...] 
 symbol can be used to indicate repeating values.
} 

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

@subsection[#:tag "conditionals"]{Conditionals and Loops}

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

@subsection[#:tag "logic"]{Predicates and Logic}

@defproc[(list? [v any]) boolean?]{
Returns True if @racket[v] is a list.
}

@defproc[(list-of? [pred? fn?] [xs list?]) boolean?]{
Returns True if @racket[pred?] is true for all elements in @racket[xs].
}

@defproc[(null? [v any]) boolean?]{
Returns True if @racket[v] is @racket[Null], where Null is defined as the empty list
@racket['()].
}

@defproc[(number? [v any]) boolean?]{
Returns True if @racket[v] is a number.
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

@defproc[(string? [v any]) boolean?]{
Returns True if @racket[v] is a string.
}

@defproc[(fn? [v any]) boolean?]{
Returns True if @racket[v] is a function.
}

@defproc[(atom? [v any]) boolean?]{
Returns True if @racket[v] is an atom: ie. a number, symbol, or function,
rather than a list or Null.
}

@defproc[(lat? [l any]) boolean?]{
Returns True if @racket[l] is a list composed solely of atoms.
}

@defproc[(any? [v any]) boolean?]{
Always returns True regardless of value of @racket[v].
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

@defproc[(apply [fun fn?] [v any] ... [lst list?]) any]{
Applies @racket[fun] to the given arguments, as if it had been called with
@racket[(fun v ... x ...)] where the @racket[x]s are the elements in @racket[lst].
}

@subsection[#:tag "io"]{Input and Output}

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

@defthing[eof eof-object?]{
A value (distinct from all other values) that represents an end-of-file.
}

@defproc[(eof? [v any?]) boolean?]{
Returns @racket[True] if @racket[v] is an @racket[eof] object.
}

@subsection[#:tag "lists"]{Lists}

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

@defproc[(map [fun fn?] [l list?]) list?]{
Given a single-argument function @racket[fun], returns a list with @racket[fun]
applied to each item in @racket[l].
}

@defproc[(filter [fun fn?] [l list?]) list?]{
Given a predicate @racket[fun], returns a new list containing only those
elements of @racket[l] for which @racket[fun] returns True.
}

@defproc[(len [l list?]) number?]{
Returns the number of items in @racket[l].
}

@defproc[(foldr [fun fn?] [base any] [l list?]) any]{
Given a function @racket[fun] with two arguments, returns the cumulative result
of @racket[fun] being applied to consecutive pairs, starting from @racket[base]
and the rightmost element of @racket[l].
}

@defproc[(foldl [fun fn?] [base any] [l list?]) any]{
Similar to @racket[foldr], except that it combines pairs from the left, starting
with the head of @racket[l] and @racket[base].
}

@defproc[(reverse [l list?]) list?]{
Returns a list with the order of @racket[l] reversed.
}

@defproc[(index [n number?] [l list?]) any]{
Returns the @racket[n]th entry of @racket[l], indexed from 1.
}

@defproc[(index* [l list?] [dims number?] ...) any]{
Walks through nested lists according to the given @racket[dims], essentially
finding index recursively for an arbitrary number of dimensions. For example,
given a nested list three lists deep, @racket[(index* l 2 3 1)] would return the
1st element of the third element of the 2nd lst, like so:
@myexamples[
  (def dave '(1 (2 3 (4 5)) 6))
  (index* dave 2 3 1)
]
Also, @racket[(l dims ...)] can be used as a shorthand for @racket[index*]:
@myexamples[
  (def dave '(1 (2 3 (4 5)) 6))
  (dave 2 3 1)
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

@defproc[(sort [fun fn?] [l list?]) list?]{
Sorts list @racket[l] according to the comparison function @racket[fun].
}

@defproc[(zip [l1 list?] [l2 list?]) list?]{
Returns a new list of lists combining @racket[l1] and @racket[l2]. Excess length
of either list is dropped.
}

@defproc[(zipwith [fun fn?] [l1 list?] [l2 list?]) list?]{
Returns a new list, combining the matching pairs of each list with @racket[fun].
Excess length of either list is dropped.
}

@defproc[(flatten [lst list?]) list?]{
Traverses a list, and flattens any nested lists into a single one-dimensional list.
}

@subsection[#:tag "strings"]{Strings}

@defproc[(=$ [x string?] [y string?]) boolean?]{
Returns True if the two strings are equal.
}

@defproc[(& [str string?] ...) string?]{
Concatenates its arguments into a single string.
}

@defproc[(list$ [str string?]) string?]{
Returns a list of one-character strings from the given string.
}

@defproc[(str$ [n any?]) string?]{
Converts a value @racket[n] to a string.
}

@defproc[(chr$ [n number?]) string?]{
Converts a given number @racket[n] to single-character string. If the number is not an 
integer it will first be coerced to one with @racket[int].
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

@defproc[(format$ [template string?] [value any?] ...) string?]{
Given a string template, returns a new string with instances of glyph @racket["#_"] replaced
in order, starting with the first value given following the string.
}

@subsection[#:tag "math"]{Math}

@defproc[(+ [x number?] ...) number?]{
Adds the given numbers left to right and returns the result. If only one argument is given,
returns the argument. If no arguments are provided, returns 0.
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

@defproc[(int [x number?]) number?]{
Returns the value of @racket[x], rounded to a whole number, rounded down.
}

@subsection[#:tag "random"]{Random Numbers}

Heresy’s random number generator operates slightly differently to traditional
BASIC's, in order to offer a more functional approach. Rather than defining a
single global seed which the RND function then refers to, Heresy's
@racket[randomize] returns a "generator" function with a given seed, allowing
one to name and seed as many generators as one needs, though for practical
purposes a default RND is still provided which is automatically created and
seeded with a derivation of the current time in milliseconds.
Heresy's RNG employs a fairly strong 64ish-bit Xorshift* algorithm, though no 
guarantees are offered as to its cryptographic security. 

@defproc[(randomize [seed any/c timer]) fn?]{
Returns a new generator function initialized with @racket[seed], which is first
passed through @racket[equal-hash-code]. If no @racket[seed] is provided, defaults
to @racket[timer].
}

@defproc[(rnd) number?]{
A pre-defined generator which returns a random number between @racket[0] and
@racket[1], exclusive, seeded by @racket[timer].
}

@defthing[timer number?]{
A special internal variable which contains the current time in milliseconds.
}

@subsection[#:tag "things"]{Things}

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

Things can also optionally be given
@italic{@hyperlink["http://wiki.c2.com/?PredicateTypes"]{predicate types}}. Predicate typing is
a form of typing in which types are defined by a predicate function, in other words
a function which given a value, will return either true or false. In this way,
any kind of type validation can be specified so long as it can be programmed as a
function which returns a Boolean value. Things check their values against these types
at both declaration, when the object is first described or instantiated, and at
assignment of new values, ie. when the copy syntax is used to generate a new thing
from the old one. If you attempt to describe or copy a thing whose values do not match
its predicate types, the program will throw an error and indicate what field did not
match its type.

@defform*[#:literals (extends inherit)
          [(describe Name)
           (describe Name (field [(type? args ...)] value) ...)
           (describe Name
                     extends super-thing
                     (field [(type? args ...)] value) ...)
           (describe Name
                     extends super-thing
                     inherit (id ...)
                     (field [(type? args ...)] value) ...)]]{
Defines a new type of Thing, given @racket[Name]. By convention, Things are
generally named in uppercase, though this is not required by the syntax. Each
field is an internal name and external symbol, which is mapped to the given
value. Anonymous functions (@racket[fn]) can be assigned as values to Thing
fields, and those functions can access the fields of the Thing by name.

Optionally, after the field name, a type predicate can be provided. Type
predicates are automatically "curried", ie. treated as a partial function
with the initial arguments following the given @racket[type?], and expecting
the result to be a single argument function that returns @racket[True] or
@racket[False]. Things which are not given a type are automatically given
the type @racket[any?], which returns @racket[True] for any value. 

If the @racket[extends] option is provided, the new Thing extends
@racket[super-thing], inheriting it's fields and methods (unless they are
overridden).  If the @racket[inherit] option is provided with it, then the
@racket[id]s are available as bindings within the method expressions. Typed
things can be extended from untyped things and vice versa; the fields from
the parent will inherit their types from the parent, unless overridden by
creating a new field with the same name and a new type signature (or no
signature, as the case may be). Note that parent things are @italic{never}
modified by their children.

@myexamples[
 (describe Project
           (name   "Destroy the world")
           (id     90)
           (budget 432000000))
 (Project 'budget)
 (describe Employee
           (name     (string?) "Dave")
           (id       (number?) 42)
           (dept     (symbol?) 'it)
           (projects (list-of? number?) '(23 90 45)))
 (Employee 'name)
 (Employee '(* * "sales" *))
 (def fn age-req? (age) (and (< 17 age) (> 45 age)))
 (describe Henchman extends Employee
           (weapon (symbol?)  'AK-47)
           (age    (age-req?) 64))
]
}

@defform*[#:literals (extends inherit)
          [(thing)
           (thing (field [(type? args ...)] value) ...)
           (thing extends super-thing
                  (field [(type? args ...)] value) ...)
           (thing extends super-thing
                  inherit (id ...)
                  (field [(type? args ...)] value) ...)]]{
Just like @racket[fn] produces an anonymous function, @racket[thing] produces an
anonymous Thing.
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

@defproc[(thing? [v any?]) boolean?]{
Returns @racket[True] if @racket[v] "looks like" a Thing, or @racket[False] if it doesn't.
@racket[thing?] employs a duck-typing method, checking the object for the expected
properties of a Thing, so it is possible, albeit unlikely, to fool it. Specifically
it checks first if @racket[v] is a @racket[fn?], then checks the returns for the default
internal methods of all Things, and its internal hash value. 
}

@defproc[(is-a? [Type thing?] [Thing thing?]) boolean?]{
Returns @racket[True] if @racket[Thing] is an instance of @racket[Type]. This
will return @racket[True] if @racket[Thing] is the same kind as @racket[Type], or
if @racket[Thing] is derived from @racket[Type], as by @racket[extends]. This is done by
comparing the internal @racket['__ident] field of @racket[Type] to both the @racket['__ident]
and @racket['__parents] fields of @racket[Thing].
}

@defproc[(thing=? [thing1 thing?] [thing2 thing?]) boolean]{
Returns @racket[True] if @racket[thing1] and @racket[thing2]'s fields are @racket[equal?]
to each other, according to the internal hash values generated from their fields, after first
checking that both things are the same type according to @racket[is-a?].
}

@defform[(Self ....)]{
@racket[Self] is the self-referring identifier for a Thing, allowing for
functions within Things to call the Thing itself. Note that if it is only the
values of the other fields, this is not necessary, as fields are defined as
local names within the context of the Thing, and thus can be referred to simply
by name.
}

@defidform[extends]{can only be used within a @racket[describe] or @racket[thing] form.}
@defidform[inherit]{can only be used within a @racket[describe] or @racket[thing] form.}

@subsection[#:tag "theory"]{Theory}

@defproc[(Y [fn fn?]) fn?]{
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

@defproc[(Y* [fn fn?]) fn?]{
A generalization of the Y-combinator that allows the function to take any number
of arguments.
}

@defform[(fnlet name args body ...+)]{
Equivalent to @racket[(Y* (fn (name) (fn args body ...)))].
For example, to map the Fibonacci sequence without
defining a named function to do it:
@myexamples[
  (map (fnlet fib (n)
         (select
          ((zero? n) 0)
          ((one? n) 1)
          (else (+ (fib (- n 2)) (fib (- n 1))))))
       (range 0 to 20))
]}

@defproc[(partial [fun fn?] [arg any] ...) fn?]{
Returns a function with the @racket[arg]s partially applied to @racket[fun],
which can then be passed the remaining arguments, as many as needed to complete
the calculation. For example:
@myexamples[
  (map (partial + 2) (range 1 to 4))
]}

@defproc[(compose [fn1 fn?] [fn2 fn?]) fn?]{
Returns a new function which is a composition of @racket[fn1] and @racket[fn2].
This function evaluates @racket[fn2] with its arguments, and then applies
@racket[fn1] to the result of @racket[fn2].
@myexamples[
  (def abs-sub (compose abs -))
  (abs-sub 4 5)
]}

@defproc[(identity [v any?]) any?]{
Returns @racket[v].
}

@subsection[#:tag "pipes"]{Pipe/Threading Operators}

@defproc[(:> [initial-value any] [fns fn?] ...) any]{
The forward pipe operator. Given a value and a series of single-argument functions, 
applies them in order from left to right and returns the resulting value.
@myexamples[
	(:> 5 inc dec sgn)
]
}

@defform[(f> fun args* ...)]{
A currying macro. Expands into an anonymous function that takes a single argument,
and inserts it as the first argument of @racket[fun], followed by the remaining
@racket[args*].
@myexamples[
	(:> '(1 2) (f> append '(3 4)))
]
}

@defform[(l> fun args* ...)]{
The inverse of @racket[f>]. Returns a function whose argument is placed as the last
argument to the given @racket[fun].
@myexamples[
	(:> '(1 2) (l> append '(3 4)))
]
}

@defform[(-> initial-value (fun args* ...) ...)]{
The first-argument threading macro. Works similarly to @racket[:>], except that it 
automatically applies @racket[f>] to each listed form following the initial value.
@myexamples[
(-> '(1 2 3 4)
    (left 2)
    (append '(a b)))]
}

@defform[(->> initial-value (fun args* ...) ...)]{
The last-argument (as in @racket[l>]) version of @racket[->]. 
@myexamples[
(->> '(1 2 3 4)
     (map (fn (x) (* x x)))
     (append '(a b)))
]
}

@subsection[#:tag "holes"]{Holes}

Holes are a simple mutable data structure based on Racket boxes, with an API inspired by
Clojure's atoms. Their purpose is to provide an in-memory data store that is treated as a
first-class value, which thus can be bound to a value or passed to functions. They can also
be useful for providing a source of shared program state.

@defproc[(hole [v any?]) hole?]{
Creates a hole containing @racket[v].
}

@defproc[(hole? [v any?]) boolean?]{
Returns @racket[True] if @racket[v] is a hole.
}

@defproc[(deref [hol hole?]) any?]{
Returns the current value contained within @racket[hol].
}

@defproc[(reset [hol hole?] [new-val any?]) hole?]{
Resets the current value of @racket[hol] to @racket[new-val], returning the hole.
}

@defproc[(update [hol hole?] [fn fn?] [args any?] ...) hole?]{
Resets the current value of @racket[hol] to the result if applying @racket[fn] to the current
value of @racket[hol], followed by @racket[args], ie. @racket[(apply f curr-val args)].
}

@defform[(reset-thing [hol hole?] (field value) ...)]{
Resets the fields of a Thing contained in @racket[hol] to the values provided, and returns
the hole.
}

@defproc[(hole-bind [hol hole?] [fn fn?]) any?]{
Applies @racket[fn] to the value contained by @racket[hol]. The monadic bind (>>=) operator for holes.
}

@defproc[(hole-guard [test boolean?]) any?]{
The monadic guard operator for holes. Primarily of use for the @racket[hole-do] DSL.
}

@subsection[#:tag "maybe"]{Maybe}

Maybe is an "option type", similar to that found in languages like Scala, Haskell, and Rust. It
allows for safe return from a function that might not return a result, without relying on @racket[Null].
A Maybe can be either a Some containing a value, or the empty thing None. Maybe is implemented
as a heirarchy of things, and the usual thing functions and behaviors apply to them, but a number
of helper functions have also been provided for easier use with them.

@defthing[Maybe thing? #:value (thing)]{
The parent object of the Maybe family. 
}

@defthing[Some maybe? #:value (thing extends Maybe (contains Null))]{
The thing for a Maybe containing a value. Child of @racket[Maybe].
}

@defthing[None maybe? #:value (thing extends Maybe)]{
The empty value, for a Maybe that contains no value. 
}

@defproc[(some [v any?]) is-some?]{
Returns @racket[v] wrapped in @racket[Some].
}

@defproc[(is-some? [opt any?]) boolean?]{
Returns @racket[True] if @racket[opt] is @racket[Some].
}

@defproc[(is-none? [opt any?]) boolean?]{
Returns @racket[True] if @racket[opt] is @racket[None].
}

@defproc[(maybe? [opt any?]) boolean?]{
Returns @racket[True] if @racket[opt] is a @racket[Maybe].
}

@defproc[(maybe-bind [opt maybe?] [fn fn?]) (or is-none? any?)]{
The bind operator for @racket[Maybe]. Returns @racket[None] if @racket[opt] is @racket[None],
or if it is @racket[Some], returns the result of @racket[fn] applied to the value field of
@racket[Some].
}

@defproc[(get-some [opt maybe?]) (or is-none? any?)]{
If @racket[opt] is @racket[Some], returns the value it contains, or else @racket[None].
}

@defproc[(maybe-map [fn fn?] [opt maybe?]) maybe?]{
Returns the result of @racket[fn] applied to the value contained in @racket[opt].
}

@defproc[(maybe-filter [pred? fn?] [opt maybe?]) maybe?]{
If @racket[pred?] is true for the value contained in @racket[opt], returns @racket[opt],
 else returns @racket[None].
}

@defproc[(maybe-guard [test boolean?]) maybe?]{
The monad guard operator for maybe. If @racket[test] is true, returns @racket[(some Null)],
else returns @racket[None].
}

@subsection[#:tag "monad-do"]{Monads and Do Notation}

@racket[monad-do] provides a generic, specializable DSL for handling monadic values, inspired by
Haskell's do notation and Scala's for comprehensions. @racket[monad-do] itself is generic,
expecting the provision of functions for the bind (@racket[>>=]), return, and guard operators, but
individual types can easily layer over this with a simple macro to provide a specialized version
of the DSL for a particular data type.

@defform/subs[#:literals (<- = if yield)
              (monad-do (bind return guard) exprs ... final-expr)
              [(exprs (name <- val)
                      (name = val)
                      (if test)
                      (expr ...))
               (final-expr (yield val ...)
                           (return-expr ...))]]{
The main implementation for do notation. The opening clause is a list of the three necessary
operators for a given type to implement monadic operations, which should be implemented
as follows:

@itemlist[@item{@racket[bind]: A function which takes two arguments: an instance of the type,
                and a function. @racket[bind] returns the result of applying the function
                to the value of the instance.}
          @item{@racket[return]: A function which takes a value, and wraps it in an instance of
                the type.}
          @item{@racket[guard]: A function which takes a boolean, and on true returns an
                instance of the type, and on false returns the empty instance or Null.}]

The rest of the body of the form is composed of various operations, which bind, guard, or return
values, described as follows. The last line of the do notation is special, in a sense, as it must
consist of either @racket[yield] or a bare expression.

@specsubform[(name <- val)]{
 Binds @racket[val] to @racket[name]. @racket[val] must be an instance of the type over which
 the do form operates.
 }

@specsubform[(name = val)]{
 Wraps @racket[val] in the current type, and binds it to @racket[name].
 }

@specsubform[(if test)]{
 Filters the ongoing expression according to test. 
 }

@specsubform[(yield val ...)]{
 When used as the last line of a do form, returns the given @racket[val](s) wrapped in the type
 of the ongoing do form.
 }

@specsubform[(expr ...)]{
 When used in the body of a do form, the @racket[expr] is evaluated but its return value ignored.
 If the last line of the do form is a bare expression, then the form will return the result of the
 expression.
 }
}

@defform[(maybe-do expr ...)]{
A specialization of @racket[monad-do] for @racket[Maybe]. This is useful for chaining operations
that return @racket[Maybe], as the monad for @racket[Maybe] short-circuits. If one operation
in the chain is a @racket[None], then the result of a @racket[yield] will be none.

@myexamples[
 (is-none? (maybe-do
            (a <- (some 5))
            (b <- None)
            (c = (+ a b))
            (yield c)))
 ]
}

@defform[(list-do expr ...)]{
A specialization of @racket[monad-do] for lists. @racket[list-do] flatmaps over its operations
forming a single-dimensional list from its calculations. This essentially enables list
comprehensions.

@myexamples[
(list-do
 (rank <- (append (range 2 to 10) '(J Q K A)))
 (suit <- '(♠ ♣ ♥ ♦))
 (if (equal? suit '♦))
 (card = (format$ "#_#_" rank suit))
 (yield card))
 ]
}

@defform[(id-do expr ...)]{
The Identity monad as a specialization of @racket[monad-do]. This essentially replaces the
functionality of the old "monadish" DSL from Heresy 0.1.0 and earlier. Mostly this is useful
as an example, but can be used for chaining together operations and mock-mutable behavior.

@myexamples[
(id-do
 (x = 5)
 (y = 4)
 (z = (+ x y))
 (print (format$ "#_ + #_ = #_" x y z)))
 ]
}

@defform[(hole-do expr ...)]{
A specialization of @racket[monad-do] for holes. Allows you to operate over and combine values 
from multiple holes easily, while returning a new hole for future use. 

@myexamples[
  (deref
   (hole-do
   (x <- (hole 5))
   (y <- (hole 6))
   (z = (+ x y))
   (yield z)))
 ]
}

@subsubsection[#:tag "implementing-monad"]{Implementing a monad}

A "monad" is a data type which can contain a value, and a set of operator functions which
operate on that type while obeying certain rules. You can think of them as a kind of container,
and the components of an assembly line that processes the container and its contents.

Let's say that we have a Thing called @racket[Box], defined thusly:
@myexamples[
(describe Box (val Null))
]
We then define a set of three functions, that work with @racket[Box]. The first, is
@italic{return}, which is a constructor function that wraps a value in our type:
@myexamples[
(def fn box-return (val)
  (Box (list val)))
 ]
The next function is @italic{bind}, known in some languages as the operator @racket[>>=]. This
takes an instance of our type, and a function, and applies the function to the value inside
our type. The definition of @italic{bind} for @racket[Box] looks like this:
@myexamples[
(def fn box-bind (box fn)
  (fn (box 'val)))
 ]
The final function is @italic{guard}, which is not especially useful on its own, but enables us
to implement a filter effect inside @racket[monad-do]. This function takes a boolean value,
the result of some test, and returns either an instance of our type with empty contents, or
nothing. For @racket[Box], it looks like this.
@myexamples[
(def fn box-guard (test)
  (if test then (box-return Null) else Null))
 ]
Now, we can provide those functions to @racket[monad-do] ourselves, or for convenience, we can
define a macro that wraps @racket[monad-do] without new operators pre-defined. It is necessary
to use @racket[def macroset] here, due to the peculiarities of the underlying Racket macro
system.
@myexamples[
(def macroset box-do
  [(_ e ...)
   (monad-do (box-bind box-return box-guard) e ...)])
 ]

Together, these three functions actually form an implementation of the Identity monad, and
by combining these and providing them to @racket[monad-do], we can 
already perform imperative-like operations in our otherwise functional language of Heresy,
and all without any mutability involved! Behold:
@myexamples[
(do
  (describe Box (val Null))
  (def fn box-return (val)
    (Box (list val)))
  (def fn box-bind (box fn)
    (fn (box 'val)))
  (def fn box-guard (test)
    (if test then (box-return Null) else Null))
  (def macroset box-do
    [(_ e ...)
     (monad-do (box-bind box-return box-guard) e ...)]))
(box-do
 (a <- (box-return 5))
 (print a)
 (a <- (box-return 10))
 (b = (* a 5))
 (print b))
 ]
