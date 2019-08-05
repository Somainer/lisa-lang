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
* "s" => String
* false => Bool
* () => Nil

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
```

## Interact with JavaScript

Lisa can interact with javascript via ScriptEngine in JVM.

To use that, you must import that environment first.

The javascript engine will NOT be initialized until you import it.

```scheme
(import-env! 'javascript)
(js "
    var a = 2, b = '3'
")

(get-js 'a) ; => 2
(get-js 'c "not found") ; => "not found"
(set-js! 'a 1) ; a becomes 1 now.

;If you want mix js in current environment
(import-env! 'javascript-cross)

a ; => 1
b ; => "3"
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

## Great! How to use it?

`sbt pack`
In target/pack/bin

`./main` to enter REPL

`./main <filename>` to execute a file.

`./main repl <filename>` To load the file before entering repl.

Also, there are some examples in `src/test/lisa/`

If you are using VSCode, I hope you will be happy with vscode-scheme and Bracket Pair Colorizer.