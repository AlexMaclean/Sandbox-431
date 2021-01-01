#lang typed/racket

(provide type-check)

(require "types.rkt")

(define-type Ty (U 'Boolean 'Integer))
(define-type TEnv (HashTable Symbol Ty))

;; Given a R program, runs the type checker on it, throwing an error if it is invalid and retrurning
;; the type otherwise, the type gets thrown away, and the program is returned unchanged
(define (type-check [p : R]) : R
  (type-check-exp (Program-body p) #hash())
  p)

;; Given an expression and a type enviroment, returns the type of the expression or throws an error
;; if the expression uses types in an invalid manner
(define (type-check-exp [e : ExpR] [tenv : TEnv]) : Ty
  (match e
    [(? integer?) 'Integer]
    [(? boolean?) 'Boolean]
    [(? symbol? var) (hash-ref tenv var (λ () (error 'Type-Errora "Unbound identifier ~e" var)))]
    ['(read) 'Integer]
    [`(- ,x) (begin (assert-type x tenv 'Integer) 'Integer)]
    [`(not ,x) (begin (assert-type x tenv 'Boolean) 'Boolean)]
    [`(,(or '+ '-) ,a ,b)
     (begin (assert-type a tenv 'Integer) (assert-type b tenv 'Integer) 'Integer)]
    [`(,(or '< '> '<= '>=) ,a ,b)
     (begin (assert-type a tenv 'Integer) (assert-type b tenv 'Integer) 'Boolean)]
    [`(,(or 'and 'or) ,a ,b)
     (begin (assert-type a tenv 'Boolean) (assert-type b tenv 'Boolean) 'Boolean)]
    [`(eq? ,a ,b) (begin (assert-type-match a b 'eq? tenv) 'Boolean)]
    [(If cnd thn els) (begin (assert-type cnd tenv 'Boolean) (assert-type-match thn els 'if tenv))]
    [(Let var val body) (type-check-exp body (hash-set tenv var (type-check-exp val tenv)))]))

;; Given 2 expressions with types that must match and a type enviroment, asserts they do and raises an
;; error otherwise, uses the in argument for the error message.
(define (assert-type-match [thn : ExpR] [els : ExpR] [in : ExpR] [tenv : TEnv]) : Ty
  (define thn-ty (type-check-exp thn tenv))
  (define els-ty (type-check-exp els tenv))
  (if (equal? thn-ty els-ty) thn-ty
      (error 'Type-Error "Types must match, but got ~a and ~a in ~e" thn-ty els-ty in)))

;; Given an expression and a type environment, and a type that it must be, raises an error if the type
;; is invalid and returns nothing otherwise
(define (assert-type [e : ExpR] [tenv : TEnv] [t : Ty]) : Void
  (define actual-t (type-check-exp e tenv))
  (unless (equal? actual-t t) (error 'Type-Error "Expected ~a, but got ~a in ~e" t actual-t e)))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit
           "utilities.rkt")

  (check-equal? (type-check (Program (Info) 5)) (Program (Info) 5))

  (check-equal? (type-check-exp 3 #hash()) 'Integer)
  (check-equal? (type-check-exp #t #hash()) 'Boolean)
  (check-equal? (type-check-exp '(read) #hash()) 'Integer)
  (check-equal? (type-check-exp 'v #hash((v . Boolean))) 'Boolean)
  (check-equal? (type-check-exp '(- 5) #hash()) 'Integer)
  (check-equal? (type-check-exp '(and #t (not #f)) #hash()) 'Boolean)
  (check-equal? (type-check-exp '(+ (- 3 5) x) #hash((x . Integer))) 'Integer)
  (check-equal? (type-check-exp '(> 8 5) #hash()) 'Boolean)
  (check-equal? (type-check-exp '(eq? 4 5) #hash()) 'Boolean)
  (check-equal? (type-check-exp (If #t #t #t) #hash()) 'Boolean)
  (check-equal? (type-check-exp (Let 'x 5 '(+ x y)) #hash((y . Integer))) 'Integer)

  (check-exn #px"Unbound identifier \'x"
             (λ () (type-check-exp '(+ 3 x) #hash())))
  (check-exn #px"Type-Error: Types must match, but got Boolean and Integer in \'if"
             (λ () (type-check-exp (If #t #f 5) #hash())))
  (check-exn #px"Type-Error: Types must match, but got Boolean and Integer in \'eq?"
             (λ () (type-check-exp '(eq? #t 5) #hash())))
  (check-exn #px"Type-Error: Expected Integer, but got Boolean in #t"
             (λ () (type-check-exp '(- #t 4) #hash()))))
