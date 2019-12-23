# Lisa

    .____    .__
    |    |   |__| ___________
    |    |   |  |/  ___/\__  \
    |    |___|  |\___ \  / __ \_
    |_______ \__/____  >(____  /
            \/       \/      \/
A naive lisp interpreter written in Scala.

Lisa is a Scheme dialect on JVM

Designed to be interactive with Scala/Java.

## Primitive Types
* 1 => Int
* 2.3 => Float
* "s" => String
* false => Bool
* () => Nil

## Composite Types

### Seq, List
Sequences and Lists can be constructed by primitive function `seq` and `list`.

* `list`: Construct a `scala.collection.immutable.List`
* `seq`: Construct a `scala.collection.immutable.Vector`

`(seq 1 2 3)` => `#Scala(Vector(1, 2, 3))`

`(.[0] s)` => Access `[0]` element in a sequence.

### Record
Record is a map-like data structure.

Construct by `record` function.
`(record 'a 1 'b 2)` => Record with key a to 1 and key b to 2.
The `a` attribute of record `s` can be accessed via:

* (.a s) in Java/Scala object style.
* (s 'a) in Lisa method calling style.

## Syntax

```scheme
(define n 0) ; Make a bound
(define (cons x y) ; Define a function
    (lambda (m) (m x y)))
(define car ; Is equal to define a lambda variable
    (lambda (m)
        (m (lambda (x y) x))))

(define (fact n)
    (define (go n x) ; Defining a function inside a function will not affact outer scope.
        (println! "Computing:" n) ; Use println! to display a string, separated by a space.
        (if (= n 0) x (go (- n 1) (* n x)))); The last expression is the return value.
    (go n 1)); Call an inner function, no tail-recursive optmize anyway.

go ; Error: Symbol go not found.

(define (< a b)
    (= (.compareTo a b) -1)); (.compareTo a b) => a.compareTo(b)

(define (fib n)
    (cond ((< n 2) n)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(let ((a 1) (b 2)) (+ a b)) ; <=>
((lambda (a b) (+ a b)) 1 2)

(define (stupid-< a b) ; When a, b >= 0
    (cond ((= a b) false)
          ((= a 0) true)
          ((= b 0) false)
          (else (< (- a 1) (- b 1)))))

(.length "hello!"); <=> "hello!".length => 6

(define eq? =) ; If you prefer eq? to =

(define-macro 
    (gen x 'for y 'in ls) ; Define a macro.
        '(map ~ls (lambda (~y) ~x)))

(gen (+ i 1) for i in (list 1 2 3)); <=>
(map (list 1 2 3) (lambda (i) (+ i 1)))


```

## Start Playing as A Calculator
Lisa is a good calculator because Integers are implemented in BigInt, and Floats are implemented in BigDecimal.
Also, Lisa supports Rational value calculation.

```scheme
(/ 2 3) ; => 2/3
(* (/ 1 2) (/ 3 4) (/ 5 6)); => 5/16
(* (/ 5 16) 0.1) ; => 0.03125
(+ (/ 1 2) (/ 3 4)); => 5/4
(int (/ 5 4)); => 1

; Calculate factorial of 100 using fact procedure defined above.
(fact 100) ; => 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000

(< 2 3 4) ;=> true
(< 2 4 3) ;=> false, because 4 is greater than 3
(<= 1 1 1 1) ; => true
```

## Interact with JavaScript

Lisa can interact with javascript via ScriptEngine in JVM.

To use that, you must import that environment first.

The javascript engine will NOT be initialized until you import it.

```scheme
(import-env! javascript)
(js "
    var a = 2, b = '3'
")

(get-js a) ; => 2
(get-js c "not found") ; => "not found"
(set-js! a 1) ; a becomes 1 now.

;If you want mix js in current environment
(import-env! 'javascript-cross)

a ; => 1
b ; => "3"
```

## Do Something Cooler
Let's write a factorial function again, in another way.
```scheme
(define (fact 0) 1)
(define (fact n) (* n (fact (- n 1))))
```
Function arguments worked with pattern matching.
```scheme
(define (eq? x x) true)
(define (eq? x y) false)
```
Remember corner cases should be defined first.
Polymorphic functions work only when you define a function with a name and
the name has already defined. If previously defined value is not a function,
 or not in the same scope,
then it will simply shadows the previous variable. Otherwise it will create a new 
polymorphic function. Note that if two functions can match same
case, the latter will never be matched.

```scheme
(define (f 0) 0)
(define (f n) (+ n (f (- n 1))))
(define (f x) 233) ; This function will never match.

(define (my-f a)
    (define (f 0) 1); This will shadow the previous f because they are not in the same scope.
    (define (f n) (* n (f (- n 1)))) ; This will create a polymorphic function.
    (f a))
(f 5) ; => 15
(my-f 5) ; => 120
```

### Match a List
Pattern `(seq <h1> <h2>)` matches a list or a vector.
`(... args)` matches the rest of the list.

```scheme
(define (sum-list (seq head (... tail))) (+ head (sum-list tail)))
(define (sum-list (seq)) 0)
```

Note that same variable means same value.

`(x (seq x x))` will match `(1 (list 1 1))` but not `(1 (list 1 2))`

### Pattern Matching Guard

The last argument in a function definition can be a pattern matching guard.
It looks like an application of `?`, `when`, or `when?`, like `(? <predicate>)`. 

* `?`, `when` requires that predicate never be failure, if so, matching will result in an evail failure.
* `when?` if predicate returns failure, this branch will be ignored and not match.

The function will match only if the guard predicate indicates true. 
And the guard *MUST* returns a Bool, otherwise will result in an `EvalFailure`.

```scheme
(define (fact 0) 1) ; Nothing interesting.
(define (fact n (? (> n 0))) (* n (fact (- n 1)))) ; Will match only when n grater than 0.
; Note that lisa has not defined > yet. You can define it as (define (> a b) (= 1 (.compareTo a b)))
(fact 5) ; => 120
(fact -5) ; => EvalFailure(No matching procedure to apply)
```

## Let's solve a problem!

```scheme
(println! "This program is to solve a hard ACM problem: a + b problem")

(define a (int (input "Please input a number:")))
(println! "Yes! a is" a)
(define b (int (input "Please input another number:")))
(println! "Yes! b is" b)
(println! "The answer is" (+ a b))
```

## Side Effects
To change a value, you should declare a mutable value by `define-mutable!`.
```scheme
(define-mutable! v)
```
If there is a variable with same name, it will shadow the previous-defined value
and be initialized with same value, otherwise it will be initialized with `()`.

Then you can change the value by `set!`, but you may not change immutable values.

To illustrate that, let's write a counter.
```scheme
(define (make-counter init)
    (define-mutable! init)
    (lambda ()
        (set! init (+ init 1))
        init))
(define c (make-counter 0))
(c); => 1
(c); => 2
(set! c 3) ; set! Error: Can not assign an immutable value c.
```

## Do Imperative Programming
Since mutable values are available, it is possible to do imperative programming.
You can use `(block <expression>*)` to introduce a new environment scope, it will do a list of expressions
and return last expression as result.
`(group! <expression>*)` will do the same thing, besides it will not introduce a new scope, and all changes are
made in the same scope.

`(while <predicate> <expression>)` macro will introduce a loop,
do body while predicate is true.

Let's write a factorial function.
```scheme
(define (fact n)
    (define-mutable! result)
    (set! result 1)
    (define-mutable! i)
    (set! i 1)
    (while (<= i n)
        (block
            (set! result (* result i))
            (set! i (+ i 1))))
    result)
```

You can even write a for-loop macro.
```scheme
(define-macro (for v from pred step body)
    '(group!
        (define-mutable! ~v)
        (set! ~v ~from)
        (while ~pred 
            (group!
                ~body
                (set! ~v ~step)))))

(for i 0 (< i 100) (+ i 2) (prtinln! i))
```

## Syntax Sugars
### Anonymous Function Literal

Anonymous functions can be created by `&` before an expression.
Variables starts with `#` and follows with a number will be captured as bound
variables. Ordered by its number, variable with only `#` will always be the first 
bound variable.

```scheme
&(+ #1 #2) ; Equals to
(lambda (#1 #2) (+ #1 #2))

&(* # #) ; ==>
(lambda (#) (* # #))

&(- #3 #1) ; ==>
(lambda (#1 #3) (- #3 #1)) ; Unlike Elixir, missing numbers are not checked!
```

The secondary usage is to limit the arity of an va-arg function, like `+`.
The syntax looks like `&<func_name>/<arity>`.
`&+/2` will be compiled to `(lambda (arg0 arg1) (+ arg0 arg1))`, limiting the arity helps to implement currying, because
it will be possible to get function arity via `length` primitive procedure.
**Note** that an anonymous function with no bound arguments will be treated as a constant function, which receives arbitrary arguments
and returns a constant value. E.g. `&1` is equivalent to `(lambda ((... _)) 1)`.

### Macros
Defining a macro in Lisa is similar to defining a closure.
`(define-macro (name args*) body*)` 
Macros are dynamic scoped. Every argument are passed without any calculation. 
Macros should eventually return a quoted expression which will be expanded where invoked.
To quote an expression, use syntax sugar `'<expression>`. `~<expression>` is the syntax sugar for unquoting an expression.

```scheme
(define-macro (unless predicate consequence alternative)
    '(if ~predicate ~alternative ~consequence))

; It is also possible to define a polymorphic macro using pattern matching.

(define-macro (reversed-apply (f x y))
    '(~f ~y ~x))
(define-macro (reversed-apply (f x))
    '(~f ~x))

(reversed-apply (- 2 3)) ; => (- 3 2) => 1
(reversed-apply (- 2)) ; => -2

; And because of pattern matching, a literal symbol in argument can be treated as a keyword.
(define-macro (is a 'equals 'to b) '(= ~a ~b))
(define-macro (is a 'not 'equals 'to b) '(if (is ~a equals to ~b) false true)) 

(is 1 equals to 1) ;=> (= 1 1) => true
(is 1 not equals to 2) ;=> (if (is 1 equals to 2) false true) => true
```

Notice that though guards are also available in macros, you can not get
real values because they care not computed, it is possible to use `eval` to actually calculate
the parameters, but duplicate calculations may occur since they must be calculated again
after the macro expansion. 
 
### Phrase Definition
```scheme
(define-phrase (args*) body*)
```

is equivalent to 

```scheme
(define-macro (`&__PHRASE__` args*) body*)
```

`&__PHRASE__` will be automatically applied if
one expression do not have a valid result and
`&__PHRASE__` macro is defined at your input.

**Note:** Unlike macro definition, defining a new phrase 
using `define-phrase` in
a new scope will not shadow previously defined phrases,
and it will create a new polymorphic variant. 

```scheme
(define-phrase (a '+ b) '(+ ~a ~b)) ; <==>
(define-macro (`&__PHRASE__` a '+ b) '(+ ~a ~b))

(3 + 2) ; Will be transformed to 
(`&__PHRASE__` 3 + 2) ; because 3 can not be applied to (+ 2)

; The more general version:
(define-phrase (a f b (when (callable? f))) '(~f ~a ~b))
; callable? is defined in prelude.lisa.

("hello" .startsWith "h") ; => (.startsWith "hello" "h") => true
```

## Do Object-Oriented Programming
Signal-based oop could be achieved by polymorphic-function.
For your information, you can see `oop.lisa`.
```scheme
(define (Person 'is-person? p (when? (&true (p 'person?)))) true)
(define (Person 'is-person? _) false)
(define (Person name age (when (and (string? name) (and (number? age) (> age 0))))) ; They are defined in prelude.lisa.
    (define-mutable! age)
    (define (age-guard age) (and (number? age) (> age 0)))
    (define (self 'name) name)
    (define (self 'age) age)
    (define (self 'age new-age (when (age-guard new-age))) (set! age new-age))
    (define (self 'setted-age new-age (when (age-guard new-age)))
        (&self (self 'age new-age)))
    (define (self 'person?) true)
    (define (self 'elder-than? other (when (Person 'is-person? other))) (> age (other 'age)))
    self)
```
With pipeline function `|>` with phrase definition in prelude, calling by chain can be achieved.
```scheme
(define-phrase (symbol (... args) (? (quoted? symbol))) '&(# ~symbol ~args)); This is defined in prelude.
(|> (Person "Elder" 91)
    ('setted-age 92)
    ('elder-than? (Person "Senpai" 24))) ; ==> true
```

## Great! How to use it?

`sbt pack`
In target/pack/bin

`./main` to enter REPL

`./main <filename>` to execute a file.

`./main repl <filename>` To load the file before entering repl.

Also, there are some examples in `src/test/lisa/`

If you are using VSCode, I hope you will be happy with vscode-scheme and Bracket Pair Colorizer.