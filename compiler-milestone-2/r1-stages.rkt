#lang typed/racket

(provide parse
         uniquify
         remove-complex-opera*)

(require "types.rkt"
         "utilities.rkt")

;; parse ---------------------------------------------------------------------------------------------

;; Given an S-expression parses it into a R AST, this is mostly the same thing but we do a little
;; bit of repackaging into structs, this pass is useful in catching any syntax errors early
(define (parse [s : Sexp]) : R
  (Program (Info) (parse-exp (match s [`(program ,_ ,exp) exp] [other other]))))

;; Given an s-expression, returns it unchanged, or raises an error if it is invalid
(define (parse-exp [s : Sexp]) : ExpR
  (match s
    [(? exact-integer? i) i]
    [(? symbol? var) var]
    ['(read) '(read)]
    [`(- ,e) `(- ,(parse-exp e))]
    [`(+ ,a ,b) `(+ ,(parse-exp a) ,(parse-exp b))]
    [`(let ([,(? symbol? var) ,v]) ,e) `(let ([,var ,(parse-exp v)]) ,(parse-exp e))]
    [other (error "Invalid expression ~e" other)]))


;; uniquify ------------------------------------------------------------------------------------------

;; Renames all the varaibles with gensym ensuring uniqueness
(define (uniquify [p : R]) : R
  (match p [(Program info body) (Program info (uniquify-exp body (make-immutable-hash)))]))

;; Recursivly descends through the R expression replacing all var occurences with a new unique name
(define (uniquify-exp [e : ExpR] [env : (Immutable-HashTable Symbol Symbol)]) : ExpR
  (match e
    [(? symbol? var) (hash-ref env var (λ () (error "Unbound identifier ~e" var)))]
    [(? exact-integer? i) i]
    [`(let ([,x ,e]) ,body)
     (let ([x* (gensym x)])
       `(let ([,x* ,(uniquify-exp e env)]) ,(uniquify-exp body (hash-set env x x*))))]
    [`(,op . ,args) `(,op . ,(map (λ (a) (uniquify-exp (cast a ExpR) env)) args))]))


;; remove-complex-opera* -----------------------------------------------------------------------------

;; Given an R program transforms it so it contains no complex opera
(define (remove-complex-opera* [p : R]) : R
  (match p [(Program info body) (Program info (rco-exp body))]))

;; Given an expression, transforms it such that it contains no complex expressions
(define (rco-exp [e : ExpR]) : ExpR
  (match e
    [(? atom? atm) atm]
    [`(let ([,x ,e]) ,body) `(let ([,x ,(rco-exp e)]) ,(rco-exp body))]
    [`(,op . ,e)
     (let ([args (map rco-arg (cast e (Listof ExpR)))])
       (env-to-lets (cons op (map (inst car ExpR) args))
                    (append-map (inst cdr ExpR (Env ExpR)) args)))]))

;; Given an expression and an environment it must run with, wraps the exression in a series of lets
;; so the exp can be evaluated without any additional env
(define (env-to-lets [exp : ExpR] [env : (Env ExpR)]) : ExpR
  (foldr (λ ([i : (Labeled ExpR)] [a : ExpR]) `(let ([,(first i) ,(second i)]) ,a)) exp env))

;; Given an R expression, that is an argument to another expression, and therefore must be an atom,
;; returns an atom and the environment needed to evaluate it
(define (rco-arg [a : ExpR]) : (Pairof ExpR (Env ExpR))
  (match a
    [(? atom? atm) `(,atm . ())]
    [`(let ([,x ,e]) ,body)
     (let ([b-arg (rco-arg body)]) (cons (car b-arg) (cons (list x (rco-exp e)) (cdr b-arg))))]
    [other (let ([tmp (gensym 'tmp)]) (cons tmp (list (list tmp (rco-exp other)))))]))
