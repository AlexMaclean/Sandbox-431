#lang typed/racket

(require "types.rkt"
         "utilities.rkt")

(provide remove-complex-opera*)

;; Given an R1 program transforms it so it contains no complex opera
(define (remove-complex-opera* [r1 : R1]) : R1
  (match r1 [(Program info body) (Program info (rco-exp body))]))

;; Given an expression, transforms it such that it contains no complex expressions
(define (rco-exp [e : ExpR1]) : ExpR1
  (match e
    [(? atom? atm) atm]
    [`(let ([,x ,e]) ,body) `(let ([,x ,(rco-exp e)]) ,(rco-exp body))]
    [`(,op ,e ...)
     (let ([args (map rco-arg (cast e (Listof ExpR1)))])
       (env-to-lets (cast (cons op (map (inst car ExpR1) args)) ExpR1)
                    (append-map (inst cdr ExpR1 (Env ExpR1)) args)))]))

;; Given an expression and an environment it must run with, wraps the exression in a series of lets
;; so the exp can be evaluated without any additional env
(define (env-to-lets [exp : ExpR1] [env : (Env ExpR1)]) : ExpR1
  (match env
    ['() exp]
    [(cons fst rest) `(let ([,(first fst) ,(second fst)]) ,(env-to-lets exp rest))]))

;; Given an R1 expression, that is an argument to another expression, and therefore must be an atom,
;; returns an atom and the environment needed to evaluate it
(define (rco-arg [a : ExpR1]) : (Pairof ExpR1 (Env ExpR1))
  (match a
    [(? atom? atm) `(,atm . ())]
    [`(let ([,x ,e]) ,body)
     (let ([b-arg (rco-arg body)]) (cons (car b-arg) (make-env x (rco-exp e) (cdr b-arg))))]
    [other (let ([tmp (gensym 'tmp)]) (cons tmp (make-env tmp (rco-exp other) '())))]))

