#lang typed/racket

(require "types.rkt")

(provide verify)

(define (verify [s : Sexp]) : R1
  (match s
    [`(program ,info ,exp) (Program '() (verify-exp exp))]
    [other (error "Invalid Program")]))

(define (verify-exp [s : Sexp]) : ExpR1
  (match s
    [(? exact-integer? i) i]
    ['(read) '(read)]
    [`(- ,e) `(- ,(verify-exp e))]
    [`(+ ,a ,b) `(+ ,(verify-exp a) ,(verify-exp b))]
    [(? symbol? var) var]
    [`(let ([,(? symbol? var) ,v]) ,e) `(let ([,var ,(verify-exp v)]) ,(verify-exp e))]
    [other (error "Invalid expression")]))

