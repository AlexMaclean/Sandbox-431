#lang typed/racket

(require "types.rkt" "utilities.rkt")
#;(
(require "types.rkt"
         "r1-stages.rkt"
         "type-check.rkt")

(define-type Environment (HashTable Symbol Value))
(define-type Value (U Boolean Integer))

(define (interpret [args : (Vectorof String)]) : Void
  (match args
    [(vector input output) ]))

(define (top-interp [s : Sexp]) : Value
  (interp-exp (Program-body (type-check (parse s))) #hash()))

(define (interp-exp [e : ExpR] [env : Environment]) : Value
  (match e
    [(or (? boolean? v) (? integer? v)) v]
    [(? symbol? var) (hash-ref env var (λ () (error "Unbound ID:" var)))]
    [(Let var val body) (interp-exp body (hash-set env var (interp-exp val env)))]
    [`(if ,cnd ,thn ,els) (if (interp-exp cnd env) (interp-exp thn env) (interp-exp els env))]
    [`(and ,a ,b) (and (interp-exp a env) (interp-exp b env))]
    [`(or ,a ,b) (or (interp-exp a env) (interp-exp b env))]
    [`(read) (cast (read) Integer)]
    [`(not ,x) (not (interp-exp x env))]
    [`(- ,x) (- (cast (interp-exp x env) Integer))]
    [`(,op ,a ,b) ((prim-binop op) (cast (interp-exp a env) Integer)
                                   (cast (interp-exp b env) Integer))]))

(define (prim-binop [s : Symbol]) : (Integer Integer -> Value)
  (match s
    ['+ +]['- -]['eq? equal?]['< <]['> >]['<= <=]['>= >=]))
)



