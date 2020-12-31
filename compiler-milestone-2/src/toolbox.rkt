#lang racket

(provide unparse)

(require "types.rkt" "utilities.rkt")

(define (unparse p)
  (match p [(Program _ e) (unparse-exp e)]))

(define (unparse-exp e)
  (match e
    [(? atom? x) x]
    [(LetR var val e) `(let ([,var ,(unparse-exp val)]) ,(unparse-exp e))]
    [`(,op . ,args) `(,op  . ,(map unparse-exp args))]))
