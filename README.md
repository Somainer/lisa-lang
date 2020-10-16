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

## Composite Types

### Seq, List
Sequences and Lists can be constructed by primitive function `seq` and `list`.

* `list`: Construct a `List`
* `seq`: Construct a `scala.collection.immutable.Vector`
* `cons` Construct a `List` like all lisp dialects do.

`(seq 1 2 3)` => `#Scala(Vector(1, 2, 3))`

`(.[0] s)` => Access `[0]` element in a sequence.
Pairs are equivalent to Lists in Lisa.
`(list 1 2)` = `(cons 1 2)` = `(cons 1 (cons 2 ()))`

If you want to access element by index, use `nth`. 
If index is out of bounds, throwing `IndexOutOfBoundsException`.
`(nth s 1)` => Access `1` element in a sequence.
Or using `get` which will return `()` when index being out of bounds.

`()` equals to empty-list `'()` in most lisp dialects, so does lisa.
In most cases, `()` will be treated as empty list, but `'()` is not the same reference
as `()`. So you can still distinguish them via `same-reference?`.

### Record
Record is a map-like data structure.

Construct by `record` function.
`(record 'a 1 'b 2)` => Record with key a to 1 and key b to 2.
The `a` attribute of record `s` can be accessed via:

* (.a s) in Java/Scala object style.
* (s 'a) in Lisa method calling style.
* (get s "a")

get procedure can be called as `(get map key)` `(get map key not-found)`

To convert a `Map` to `record`, use `->record` procedure.

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
    (go n 1)); Call an inner function.

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
          (else (stupid-< (- a 1) (- b 1)))))

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

## Builtin Procedures and Macros
Definitions can be found in `lib/lisa-predefs`.
They are definitions signatures with doc strings. 
Some of them have a implementation illustrating the general idea or procedure of that function.
Initially, the interpreter imported the predef environment. 
They are definitions in `primitives.lisa`, `testers.lisa` and `collections.lisa`.

## Modules & Imports
To use variables defined in other files, `import!` macro could be your option.
Import is relative, meaning the file path is totally based in the written file.

Hence we have a file: `module.lisa`.
```scheme
;; module.lisa
(define a 114)
(define b 514)
```

If we are writing a file in the same directory, we want to use a, we could:

```scheme
(import! module)
;; Or
(import! "module")
;; Or
(import! "./module.lisa")
;; Or 
(import! "./module")

(println! a)
```

The first argument of `import!` can be a string or symbol, indicating the relative path of the source file, the extension `.lisa` is optional.
In theese cases, both `a` and `b` will be available.

If we want to import some variables only, we should declare them.

```scheme
(import! module a)
(println! a)
```

If you do not want to shadow the variables in current scope. Prefixing them is an option, the syntax is as follows.

```scheme
(import! module * as mod)
(println! mod.a)
```

Also, the `require` function is provided as an option to import modules as records. Note that `require` is not a macro, which means dynamic is posssible to achieve via this function.

```scheme
(define mod (require "module"))
(println! (.a mod))
(println! (get mod 'b))
```

`require` also supports importing a `Scala` file, the last expression will be passed as the result. So it is very handy to extend `lisa` capabilities.

```scala
// GreatFn.scala
object Great {
    def greatFunction(x: Int) = x + 1
}

Great // Last expression is the return value.
```
```scala
// AsLong.scala

import moe.roselia.lisa.LispExp._ // You can import lisa internal scala classes.

// Sometimes we need to pass a long to a method, but by default
// lisa will convert numbers to Int. So here is an ad-hoc solution.
PrimitiveFunction {
  case SInteger(n) :: Nil => WrappedScalaObject(n.toLong)
}
```
```clojure
; main.lisa

(define Great (require "Great.scala"))
(.greatFunction Great 1) ; 2

(define as-long (require "AsLong.scala"))
(Thread/sleep (as-long 1000))
```

## Auto Single Abstract Method Transforming
`Lisa` supports auto SAM transforming, if a procedure, like `Closure`, `PolymorphicProcedure`, `PrimitiveFunction` is passed to a method, static method, or constructor whose
argument is a Functional Interface, `Lisa` will automatically conver the argument to the correct type.

Example:
```clojure
(define list '(1 2 3))
(.map list &(+ # 1)) ; '(2 3 4)
```
The map method of list(scala class `LisaList`) is 
```scala
class LisaList[+T] {
    def map[U](fn: T => U): LisaList[U]
}
```
As we can see, the argument type of `map` is `T => U`, which is `Function[T, U]` during runtime. If a closure (`&(+ # 1)`, which will be expanded to `(lambda (#) (+ # 1))`) does not match the type but `lisa` will automatically convert the `Closure` to the desired type via a `Proxy`.

### Construct a SAM instance
```clojure
(define empty? (new java.util.function.Predicate .isEmpty))
(define list (new java.util.ArrayList))
(.add list ())
(.add list "")
(.add list "Hello")

(define stream (.stream list))
(define empty-elements (.filter stream empty?)) ; Actually (.filter stream .isEmpty) also works
(.count empty-elements) ; 2
```
This example is to demostrate constructing a SAM instance via the SAM interface and the procedure also works.

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
(import-env! javascript-cross)

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

### Match Value Already Exists
Like `Scala`, matching a value that already exists in the environment could be achieved by
using ``` `<Symbol>` ```.
```scheme
(define x 1)
(define (is-one? `x`) true) ; Will match only when 1 is passed.
(define (is-one? _) false)
```
Note that it is an undefined behavior to mix use same symbol in plain and back-quoted.
```scheme
(define x 1)
(define (evil `x` x) x)
(evil 1 2) ; => 2
(evil 2 3) ; Match Error, (2 3) does not match (x x)
```
It sounds weird because such pattern will **not** introduce a new bound in
function environment. So please DO NOT mix use same symbol in ``` `x` ``` and `x`.

But when back-quoted symbol does not exist on the environment, it will treated same as
plain symbol. 
```scheme
(define (add-one `the argument`) (+ `the argument` 1)) 
```
This could be used to naming a symbol with illegal characters.

### Match a List
Pattern `(<h1> <h2>)` matches a list or a vector.
`(... args)` matches the rest of the list.

```scheme
(define (sum-list (head (... tail))) (+ head (sum-list tail)))
(define (sum-list ()) 0)
```

Note that same variable means same value.

`(x (seq x x))` will match `(1 (list 1 1))` but not `(1 (list 1 2))`

### Pattern Matching Guard

The last argument in a function definition can be a pattern matching guard.
It looks like an application of `?`, `when`, or `when?`, like `(? <predicate>)`. 

* `?`, `when` requires that predicate never be failure, if so, matching will result in an eval failure.
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

## Tail-call Optimization

Lisa has tail-call optimization. When the last statement of a procedure is exactly an application, such tail call will be optimized.

**Notice** that different from most tco-optimize languages, invoking `if` as last statement will not be regarded as
a tail call because `if` is not a control statement but an expression in lisa.

```scheme
(define (overflow-fact n)
    (define (f n x) 
        (if (= n 0) x (f (- n 1) (* n x)))) ; Won't be optimized here.
    (f n 1))

(overflow-fact 1000) ; => Boom!
```

Luckily, in most cases, you won't miss `if` and `cond` in lisa because of pattern matching.

```scheme
(define (fact n)
    (define (f 0 x) x)
    (define (f n x) (f (- n 1) (* x n)))
    (f n 1))

(fact 1000) ; => Can get correct result.
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
To implement `break` and `continue` capability, you need to use `returnable` function.
`returnable` brings in return ability.
```scheme
(returnable
    (lambda (return)
        (return 2)
        3)) ; => 2
```
But you **can not** use it outside returnable body.

Here is a for-loop macro example inside `prelude.lisa`:
```scheme
(define-macro (for (var init) condition update (... body))
    (define continue-sym (gen-sym))
    (define break-sym (gen-sym))
    '(returnable
        (lambda (~break-sym)
            (define (break) (~break-sym)) ; To make sure break does not accept any arguments.
            (define-mutable! ~var)
            (set! ~var ~init)
            (while ~condition
                (group!
                    (returnable
                        (lambda (~continue-sym)
                            (define (continue) (~continue-sym))
                            ~body))
                    (set! ~var ~update))))))
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

### String Interpolation
String Interpolation works in parse time. Such expression will be transformed
to a procedure application. Such strings are called template strings.

Template strings are literals enclosed by single or triple double quote characters (`"` or `"""`)
followed an identifier, such as `$"Like $this."`, or`template-name"""string literals"""`.
Note that there shouldn't be any spaces between identifier and string literal in order to
distinguish it from an application whose procedure is an identifier and the first argument
is a string like `(string "string")` is not `(string"string")`.

Template literals can contain placeholders. 
These are indicated by the dollar sign and curly braces (`${expression}`), 
Or identifiers with only letters and number after a dollar sign
(no hyphens, which are legal lisa identifiers) (`$identifier`).
The expressions in the placeholders and the text between the quotes get passed to a function.

One special template is the dollar sign `$`, this works likes normal string interpolation.
And such string will compiled to an application of `string` which joins the arguments.
`$"1 + 1 = ${(+ 1 1)}."` will be transformed to `(string "1 + 1 = " (+ 1 1) ".")`.

Otherwise the templates and placeholders will be transform to the application of that identifier.
Like `need-lisa"($name $age)"` will be compiled to `(need-lisa '("(" " " ")") (list name age))`.
So, the return type of template function does not necessarily be a string.

The stand interpolator is `string/interpolate`. Hence, the `need-lisa` procedure can be written as:
```clojure
(define (need-lisa parts args)
    (let ((code (string/interpolate parts args)))
        (read-string code)))
(define name "YJSNPI")
(define age 24)
need-lisa"($name $age)" ; => (YJSNPI 24)
```

The secondary usage is to limit the arity of a var-arg function, like `+`.
The syntax looks like `&<func_name>/<arity>`.
`&+/2` will be compiled to `(lambda (arg0 arg1) (+ arg0 arg1))`, limiting the arity helps to implement currying, because
it will be possible to get function arity via `length` primitive procedure.
**Note** that an anonymous function with no bound arguments will be treated as a constant function, which receives arbitrary arguments
and returns a constant value. E.g. `&1` is equivalent to `(lambda ((... _)) 1)`.

### Macros
Defining a macro in Lisa is similar to defining a closure.
`(define-macro (name args*) body*)` 
Macros are executed static scoped. Every argument are passed without any calculation.
Macros should eventually return an expression which will be expanded where invoked.
If you want to refer to a value dynamically, use `dynamic-resolve` macro which only works
inside macros. This macro will lookup through calling chain to resolve the symbol in
dynamic scope.
Since code is data in Lisa, you can generate code by quote and unquote or returning a list.
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

(define-macro (unless pred conseq alter)
    (list 'if pred alter conseq)) ; Can also be '(if ~pred ~alter ~conseq)
(unless (< 3 2) (println! "3 > 2") (println! "Impossible"))

(define-macro (dirty-nth ls n)
    (list (string->symbol (string ".[" n "]")) ls))
(dirty-nth '(1) 0) ; => 1
(expand-macro '(dirty-nth s 0)); => Apply(.[0], List(s))
(write (expand-macro '(dirty-nth s 0))) ; => '(.[0] s)
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

## Java Inter-Ops
`Lisa` could deal with `Scala` objects mostly since they are automatically converted to `Lisa`
objects. To convert Java objects, use pre-defined `from-java` procedure.
As shown above, variable starts with dot will be treated as method accessors.
Also, `new` function could be used to call constructor of class like `Clojure`. 
If you miss `doto` macro, you can just write one as example below.
```clojure
(define-macro (doto ex (... ops))
    (define sym (gen-sym))
    (define ops-with-obj 
        (map ops (lambda ((fn (... args))) (cons fn (cons sym args)))))
    '(let ()
        (define ~sym ~ex)
        ~~ops-with-obj
        ~sym))
```
Hence, you can write code like:
```clojure
(define array-list (doto (new ArrayList) (.add 1) (.add 2))) ; WrappedScalaObject[ArrayList]
(define wrapped-list (from-java array-list)) ; WrappedScalaObject[Iterable]
(define lisa-list (iter wrapped-list)) ; '(1 2)
```

Static members can also be accessed now.
```clojure
Math/PI ; = Math.PI
Integer/MAX_VALUE ; = 2147483647
(Math/max 1 2) ; = 2
(String/format "Hello, %s!" "World") ; => "Hello, World!"
```

### Null
In many lisp dialects, `nil`s are empty lists, so does lisa. `nil?` tests if an object is
an empty list. Since JVM has a special value `null` for no references, and we also need
it for inter-operation ability. So here is also a special value `null` in lisa. `null` is
not `nil`, and `nil?` test `null` will be `false`. The only way to test if a value `x` is null
is `(= x null)` or `(same-reference? x null)`. `null` will be translated to JVM null if it
is passed in JVM methods or reflections. But in lisa, we still use `()` to represent `nil`.

## Logical Programming (Experimental)
Lisa supports logical programming. Lisa has a built-in logical programming system.

### Atoms
Atom is a special form in Lisa Logical which indicates `atoms`. Atom starts with a `:`
and follows an identifier or a String. Like `:a`, `:"atom with spaces"`, or `:"b"`.
`:a` is equal to `:"a"`.

### Logical Module
To use logical module, you should import that via `(import-env! logical)` first.
Logical facts and rules are defined in `LogicalContext`s, hence, it is possible to deal with
multiple logical worlds. When a `LogicalContext` is stored in a variable, it is immutable. 
Use `(current-context)` to get current context, `(pop-context!)` to remove current logical
context, and `(logical/push-context context)` to set `context` as current context.
To create a context, simply use `(logical/new-context)`.

When a context is created, you can use defined macros like `fact`, `define-rule` and `query`.

### Facts
Facts are primitives in logical programming.
Facts can be defined using macro `fact`.
```clojure
(fact (path a b))
(fact (path b c))
(fact (path d e))
(fact (path e f))
```
Notice that you usually do not have variables in fact definition. So, symbols in facts are automatically
transformed to atoms. If you want to use variables, use `'sym`. This **only** works in fact definition.
`_` matches any case without introducing any new constraints into the environment.

```clojure
(fact (path a b)) ; <=> 
(fact (path :a :b))
```

### Rules
Rules are means of abstraction in logical programming.
Rules can be defined via `define-rule`.

```clojure
(define-rule (link a c)
    (or (path a c)
        (and (path a b)
            (link b c))))
```

Symbols in rules are treated as variables while atoms are treated as atoms.

### Queries
Queries are means of combination in logical programming.
A query can be combines with facts, rules and some special combinator.
A query can be executed via macro `query`.
If you do not want to do all searches, you can delay the computation via `lazy-query`,
this macro will return a `LazyList` which will not compute all results at once.
To iterate through results, use `car` and `cdr` and use `nil?` to test if
the sequence is empty like any other seq-likes in Lisa.

```clojure
(query (link from to)) ; => ({'from :a 'to :b} {'from :b 'to :c} {'from :d 'to :e} {'from :e 'to :f} {'from :a 'to :c} {'from :d 'to :f})
(query (link :a to)) ; => ({'to :b} {'to :c})
```

The execution result will be a list of records. An empty list will be emitted if no fact match the query.
If you only want to test if a fact is true, use `is-true?` macro.

### Combinator
Special combinator can help you build powerful queries.

* `and` Accepts arbitrary queries, the result query will match if all queries match. 
* `or` Accepts arbitrary queries, the result query will match if one of the queries match. 
* `but` Accepts a query, the result query will not match if this query matches.
* `?` Accepts an expression evaluated to boolean, the result query will match if the expression is true.
* `execute-lisa` Just runs the accepted expression and do nothing to the result.
* `=` is the unifier that unifies two expressions.
* `<-` Accepts one symbol and an expression, evaluate the expression and unify the result to that symbol.
* `unique` Accepts on query, the result query will match if each constraint only has exactly one match.

```clojure
(fact (salary a 114))
(fact (salary b 514))

(query (and (salary x y) (? (> y 200)))) ; => ({'x :b 'y 514})
(query (and (salary x y) (execute-lisa (println! y)))) ; Prints 114 514 and the result should be ({'x :a 'y 114} {'x :b 'y 514})

(query (= (x x x) ((1 b c) (a 2 c) (a b 3)))); => ({'a 1 'b 2 'c 3 'x (1 2 3)})

(define-rule (minus-one x y)
    (<- y (- x 1)))
(is-true? (minus-one 3 2)) ; => true
```

### Procedure Variants
Except from macros like `fact`, `query`, there also have procedure versions to make it easier to
do logical queries and add facts from variables. The name of these procedures are prefixed with verbs.

* `fact` => `add-fact`
* `query` => `run-query`
* `lazy-query` => `run-lazy-query`
* `is-true?` => `test-is-true?`

So, basically, you can write a simple query interface by writing an infinite loop of
`(println! (run-query (read)))`.

## Great! How to use it?

Install SBT

run `sbt pack`

Compiled binaries can be discovered
in target/pack/bin

`./lisa` to enter REPL

`./lisa <filename>` to execute a file.

`./lisa repl <filename>` To load the file before entering repl.

Also, there are some examples in `src/test/lisa/`

If you are using `VSCode`, I hope you will be happy with vscode-scheme and Bracket Pair Colorizer.
Also, here is a VSCode extension [lisa-vscode](https://github.com/Somainer/lisa-vscode) available. 
This extension brings in Syntax Highlight and Markdown code block support for Lisa.

## Awesome! How can I use it in production?

You won't.