#lang typed/racket

(require "types.rkt"
         "utilities.rkt")

(provide uniquify)

(define (uniquify [r1 : R1]) : R1
  (match r1 [(Program info body) (Program info (uniquify-exp body '()))]))

(define (uniquify-exp [e : ExpR1] [env : (Env Symbol)]) : ExpR1
  (match e
    [(? symbol? var) (lookup var env)]
    [(? exact-integer? i) i]
    [`(let ([,x ,e]) ,body)
     (let ([x-u (gensym x)])
       `(let ([,x-u ,(uniquify-exp e env)]) ,(uniquify-exp body (make-env x x-u env))))]
    [`(,op ,e ...)
     (cast `(,op ,@(map (λ ([e : ExpR1]) (uniquify-exp e env)) (cast e (Listof ExpR1)))) ExpR1)]))
