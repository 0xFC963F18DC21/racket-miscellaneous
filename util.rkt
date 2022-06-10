#lang racket

; For extra macro goodness.
(require (for-syntax racket/match syntax/parse))

; Macro for quickly defining transparent structures.
(define-syntax-rule (t-struct forms ...)
  (struct forms ... #:transparent))

; Clojure's two threading macros, recreated.
; Adapts Rackjure's conversion of non-parenthesised forms into calls, i.e.
; (--> 5 (+ 2) number->string) => (number->string (+ 2 5))
(define-syntax >-- ; Thread-First
  (syntax-rules ()
    [(>-- form)                         form]
    [(>-- form (sf args ...) forms ...) (>-- (sf form args ...) forms ...)]
    [(>-- form sf forms ...)            (>-- (sf form) forms ...)]))

(define-syntax --> ; Thread-Last
  (syntax-rules ()
    [(--> form)                         form]
    [(--> form (sf args ...) forms ...) (--> (sf args ... form) forms ...)]
    [(--> form sf forms ...)            (--> (sf form) forms ...)]))

; Continuation-wrapped function definition macro. This allows the use of "return".
; Adapted from https://beautifulracket.com/explainer/continuations.html
(define-syntax (define/return caller-stx)
  (syntax-case caller-stx ()
    [(_ fn-spec body ...)
     (with-syntax ([return (datum->syntax caller-stx 'return)])
       #'(define fn-spec
           (let/cc return
             (begin body ...))))]))

; Define a function that is implicitly curried.
; This is similar to how Haskell and other SML languages usually prefer their function definitions.
;
; Essentially, you get a function with multiple types depending on how it ends up being used:
;
; (define/curried (f& a b c d) (f a b c d))
;
; (A, B, C, D) -> E
; (A, B, C) -> D -> E
; A -> (B, C, D) -> E
; (A, B) -> (C, D) -> E
; (A, B) -> C -> D -> E
; A -> (B, C) -> D -> E
; A -> B -> (C, D) -> E
; A -> B -> C -> D -> E
;
; Contrived Example:
;
; (define/curried (plus/3 a b c) (+ a b c))
;
; (plus/3   1  2  3) ; => 6
; ((plus/3  1) 2  3) ; => 6
; ((plus/3  1  2) 3) ; => 6
; (((plus/3 1) 2) 3) ; => 6
;
; CAVEAT: Does not work with rest args, optional args nor keyword args.
(define-syntax (define/curried uncurried-stx)
  ; First, decompose the definition and find the name, arguments and body.
  (match-let ([(list _ (list name args ..1) body ..1) (syntax->datum uncurried-stx)])
    ; Abstract the body to make it take up less space in the function.
    (define inner-name (gensym (string-append (symbol->string name) "/inner-")))
    (define inner-body (append (list 'λ args) body))
    
    ; Now we make some lambdas out of it. It is laid out to allow any partial application.
    (define/match (prefixes list)
      [('())             '(())]
      [((list x xs ...)) (cons null (map (λ (l) (cons x l)) (prefixes xs)))])
  
    (define (proc-cases p-args)
      (let ([arg-prefs (cdr (prefixes p-args))])
        (map (λ (ags)
               (if (equal? ags p-args)
                   (list ags (cons inner-name args))
                   (list ags (cons 'case-lambda (proc-cases (remove* ags p-args))))))
             arg-prefs)))

    ; And now, putting everything together:
    (with-syntax ([f-name  (datum->syntax uncurried-stx name)]
                  [f-cases (datum->syntax uncurried-stx (cons 'case-lambda (proc-cases args)))]
                  [f-inner (datum->syntax uncurried-stx inner-body)]
                  [f-iname (datum->syntax uncurried-stx inner-name)])
      #'(define f-name (let ([f-iname f-inner]) f-cases)))))

; "Fry" an expression with holes, transforming it into an equivalent lambda.
; Frying occurs on dunders (__), examples being:
;
; (λf (* __ 2)) is equivalent to (λ (x) (* x 2))
; (λf (list __ __)) is equivalent to (λ (x y) (list x y))
; etc.
;
; This is helpful for providing lambdas for any applicative operation (mapping, filtering, etc.).
;
; This implementation of quotation frying works similarly to Scala's implementation, in that only
; the top-level S-expression's dunders are fried:
;
;                  Fried, resultant lambda is (λ (x) (* x 5)) 
;                  v
;   VALID: (λf (* __ 5))
; INVALID: (λf (* __ (+ __ 4)))
;                  ^     ^
; Successfully fried     Untouched dunder, will cause an error or unintended effects if defined
(define-syntax (λf uncooked-stx)
  ; The new bindings and S-expression for the fried quotation.
  (define bindings-found null)
  (define new-datum null)

    ; Utility for generating symbols when frying.
  (define (new-fried)
    (let ([sym (gensym "fried-")])
      (set! bindings-found (cons sym bindings-found))
      sym))

  ; Only walks the top-level expression to avoid ambiguity. Call λf multiple times to nest lambdas.
  (define (sexpr-walk datum)
    (match datum
      [(list '__ rest ...) (begin
                             (set! new-datum (cons (new-fried) new-datum))
                             (sexpr-walk rest))]
      [(list x rest ...)   (begin
                             (set! new-datum (cons x new-datum))
                             (sexpr-walk rest))]
      ['__                 (sexpr-walk (list '__))]
      ['()                 #f]
      [else                (set! new-datum (list else))]))

  ; This is where we actually walk the S-expression.
  (sexpr-walk (cadr (syntax->datum uncooked-stx)))

  ; Enrich our new bindings and S-expression with context from the given syntax, then return the
  ; newly-fried lambda.
  (with-syntax ([bs  (datum->syntax uncooked-stx (reverse bindings-found))]
                [exp (datum->syntax uncooked-stx (if (null? (cdr new-datum))
                                                     (cons 'identity (reverse new-datum))
                                                     (reverse new-datum)))])
    #'(λ bs exp)))

; Long-hand syntax for λf.
(define-syntax-rule (lambda-fried expr)
  (λf expr))

; Haskell-like flip operator. Inverts the order of arguments for a 2-arity function.
(define ((hs-flip fn) a b)
  (fn b a))

; Haskell-like curry operator. Takes a 2-arity function and curries it.
(define (((hs-curry fn) a) b)
  (fn a b))

; Haskell-like uncurry operator. Takes a 2-arity curried function and uncurries it.
(define ((hs-uncurry fn) a b)
  ((fn a) b))

; Infix-rearrangement macro. Transforms an infix application to the correct order.
; This macro assumes everything is written in infix within the caller body.
(define-syntax-rule (infix (lhs op rhs))
  (op lhs rhs))

; Convert a normal function into a continuation-passing function.
(define (fn->cont f)
  (λ args
    (let ([r (reverse args)])
      ((car r) (apply f (reverse (cdr r)))))))

; Thread forms through the continuation-passing style of functions.
; Note that every form after the first must be a lambda with a single form in its body.
; The single form must be a call to a function that is also written in continuation-passing style.
(define-syntax ~~>
  (syntax-rules ()
    [(~~> (cpf ...) rc)                     (cpf ... rc)]
    [(~~> cpfs ... (λ (argn) (cpf ...)) rc) (~~> cpfs ... (λ (argn) (cpf ... rc)))]))

; Fast integer exponentation.
; Uses the right-to-left square-multiply algorithm.
; See https://en.wikipedia.org/wiki/Exponentiation_by_squaring for more information.
(define natural/c (flat-named-contract 'natural-number (λ (n) (and (integer? n) (>= n 0)))))
(define positive/c (flat-named-contract 'nonzero-number (λ (n) (and (integer? n) (> n 0)))))

(define/contract (fast-int-exp base index [mod #f])
  (->* (integer? natural/c) (positive/c) integer?)
  (let ([modf (if mod (λ (x) (modulo x mod)) identity)])
    (define (intern-exp n acc1 acc2)
      (cond
        [(= 0 n)   (modf acc2)]
        [(even? n) (intern-exp (/ n 2) (modf (* acc1 acc1)) acc2)]
        [else      (intern-exp (- n 1) acc1 (modf (* acc1 acc2)))]))
    (intern-exp index base 1)))

; Re-implementation of Clojure's str function. Turns all objects into strings then concatenates the
; strings into one.
(define str
  (case-lambda
    [()             ""]
    [(a)            (format "~a" a)]
    [(a b)          (format "~a~a" a b)]
    [(a b c)        (format "~a~a~a" a b c)]
    [(a b c . rest) (string-append (str a b c)
                                   (foldr (λ (obj acc)
                                            (string-append (format "~a" obj) acc))
                                          ""
                                          rest))]))

; Re-implementation of Haskell's iterate function (or Clojure's since this one works on streams).
; Takes a function of type (-> T T), and some initial value of type T, and produces an infinite stream
; starting with the initial item, and repeatedly applying the function to the item and future results.
(define (iterate fn init)
  (stream-cons init (iterate fn (fn init))))

; Transducer operations. Transducers are things that map from a reduction function to another.
;
; Reduction Signatures (in this case, they are multi-arity functions):
;   Initial:    B                Provides the initial accumulator value if none was given.
;   Completion: B -> B           The finishing function that runs after all is reduced.
;   Step:       (A -> B -> B)    The forward right-reduction step function.
;
; Note that when using with transduce-l or eduction-l, the initial given reduction function's step
; function will be flipped. Please give the two aforementioned functions reduction functions with
; a step function that has a signature of (B -> A -> B).
;
; Transducers are composed with standard function composition.

; Shorthand definition for a transducer.
; The bindings rf, acc and inp are reserved in this macro, and will expand to give the following
; definition structure (the preamble is used to initialise states and the like):
;
; (define fn-spec
;   (λ (rf)
;     preamble
;     (case-lambda
;       [()        init-body]
;       [(acc)     comp-body]
;       [(inp acc) step-body]))
;
; LAWS:
; - init-body should always call rf's init excatly once.
; - comp-body should call rf's completion on acc exactly once.
; - step-body can choose how many times to call rf's step any number of times.
;
; SENSIBLE DEFAULTS (using all three will create an identity transducer):
; - init-body: (rf)
; - comp-body: (rf acc)
; - step-body: (rf inp acc)
(define-syntax (define/transducer bodies-stx)
  (syntax-parse bodies-stx
    #:datum-literals [init: comp: step:]
    [(_ fn-spec preamble ...
                init: init-body ...
                comp: comp-body ...
                step: step-body ...)
     (with-syntax ([rf  (datum->syntax bodies-stx 'rf)]
                   [acc (datum->syntax bodies-stx 'acc)]
                   [inp (datum->syntax bodies-stx 'inp)])
       #'(define fn-spec
           (λ (rf)
             preamble ...
             (case-lambda
               [()        init-body ...]
               [(acc)     comp-body ...]
               [(inp acc) step-body ...]))))]))

; This function flips the order of arguments for the step case of a reducer.
; This is to keep transduce-l working as one expects:
;
; > (define xf (compose (take/t 3) (map/t number->string)))
;
; > (transduce-l xf string-append "" '(1 2 3 4 5))
; "123"
;   ^
;  Would be "321" instead of "123" as expected if string-append is not flipped.
;
; > (transduce-r xf string-append "" '(1 2 3 4 5))
; "345"
(define/transducer flip/t
  init: (rf)
  comp: (rf acc)
  step: (rf acc inp))

; This function transforms a 2-arity function into a transduce-suitable function by adding in a
; 1-arity case. By default, it is identity.
(define (completing f [cf identity])
  (case-lambda
    [(x)   (cf x)]
    [(x y) (f x y)]))

; The following functions apply a transducer function given an initial reduction function.
; The reduction function MUST have at least a 2-arity (step) and 1-arity case (completion).
; It should also have (optionally) a 0-arity case (init) to provide an empty case.
(define transduce-l
  (case-lambda
    [(xf f lst)      (transduce-l xf f (f) lst)]
    [(xf f init lst) ((transduce-with foldl) (compose xf flip/t) f init lst)]))

(define transduce-r
  (case-lambda
    [(xf f lst)      (transduce-r xf f (f) lst)]
    [(xf f init lst) ((transduce-with foldr) xf f init lst)]))

(define (transduce-with foldf)
  (case-lambda
    [(xf f lst)      (transduce-with xf f (f) lst)]
    [(xf f init lst) (let ([red (xf f)])
                       (red (foldf red init lst)))]))

; Eductions capture the process of applying a transducer.
; It applies all of the given xforms and a final list, and applies the transducer to the list.
; This implementation returns a lambda.
(define (eduction-l . xfs)
  (define/curried (educed lst f init)
    (transduce-l (apply compose (append xfs (list flip/t))) f init lst))
  educed)

(define (eduction-r . xfs)
  (define/curried (educed lst f init)
    (transduce-r (apply compose xfs) f init lst))
  educed)

; Some transducer-compatible version of certain functions.

; Transduce Map.
(define/transducer (map/t f) 
   init: (rf)
   comp: (rf acc)
   step: (rf (f inp) acc))

; Transduce Filter. Only reduces items that match a predicate.
(define/transducer (filter/t pred)
  init: (rf)
  comp: (rf acc)
  step: (if (pred inp)
            (rf inp acc)
            acc))

; Transduce Take. Only reduces the first n items into the result.
(define/transducer (take/t n)
  (define left n)
  init: (rf)
  comp: (rf acc)
  step: (if (> left 0)
            (begin (set! left (- left 1))
                   (rf inp acc))
            acc))

; Transduce Drop. Only reduces the items past the first n items into the result.
(define/transducer (drop/t n)
  (define left n)
  init: (rf)
  comp: (rf acc)
  step: (if (> left 0)
            (begin (set! left (- left 1))
                   acc)
            (rf inp acc)))

; Debug Transducer.
(define/transducer (debug/t [prefix "DEBUG"])
  init: (rf)
  comp: (println (format "~a [ACC: ~a]" prefix acc))
        (rf acc)
  step: (println (format "~a [INP: ~a | ACC: ~a]" prefix inp acc))
        (rf inp acc))

; Of course, this is a utility library, so we want everything to be exported.
(provide (all-defined-out))
