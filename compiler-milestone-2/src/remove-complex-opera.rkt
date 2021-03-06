#lang typed/racket

(provide remove-complex-opera*)

(require "types.rkt" "utilities.rkt")

;; Given an R program transforms it so it contains no complex opera
(define (remove-complex-opera* [p : R]) : R
  (match p [(Program info body) (Program info (rco-exp body))]))

;; Given an expression, transforms it such that it contains no complex expressions
(define (rco-exp [e : ExpR]) : ExpR
  (match e
    [(? atom? atm) atm]
    [(Let x e body) (Let x (rco-exp e) (rco-exp body))]
    [(If cnd thn els) (If (rco-exp cnd) (rco-exp thn) (rco-exp els))]
    [`(,op . ,exps) (let-values ([(args envs) (rco-arg* exps)])
                      (env-to-lets (cons op args) (append* envs)))]))

;; Given an expression and an environment it must run with, wraps the exression in a series of lets
;; so the exp can be evaluated without any additional env
(define (env-to-lets [exp : ExpR] [env : (Env ExpR)]) : ExpR
  (foldr (λ ([i : (Labeled ExpR)] [a : ExpR]) (Let (first i) (second i) a)) exp env))

;; Given an R expression, that is an argument to another expression, and therefore must be an atom,
;; returns an atom and the environment needed to evaluate it
(define (rco-arg [a : ExpR]) : (Values ExpR (Env ExpR))
  (match a
    [(? atom? atm) (values atm '())]
    [(Let x e body) (let-values ([(arg env) (rco-arg body)])
                       (values arg (cons (list x (rco-exp e)) env)))]
    [other (let ([tmp (gensym 'tmp)]) (values tmp (list (list tmp (rco-exp other)))))]))

;; Like a version of rco-arg, but takes a list and returns lists of the results
(define (rco-arg* [as : (Listof ExpR)]) : (Values (Listof ExpR) (Listof (Env ExpR)))
  (for/lists ([args : (Listof ExpR)] [envs : (Listof (Env ExpR))])
                                          ([a as]) (rco-arg a)))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit "test-utilities.rkt")

  (check-match
   (remove-complex-opera*
    (Program (Info)
             '(+ 52 (- 10))))
   (Program _
            (Let tmp.1 '(- 10)
                  `(+ 52 ,tmp.1))))

  (check-equal?
   (remove-complex-opera*
    (Program (Info)
             (Let 'a 42
                   (Let 'b 'a
                         'b))))
   (Program (Info)
            (Let 'a 42
                  (Let 'b 'a
                        'b))))

  (check-equal?
   (remove-complex-opera*
    (Program (Info)
             (Let 'y (Let 'x.1 20
                            `(+ x.1 ,(Let 'x.2 22 'x.2)))
                   'y)))
   (Program (Info)
            (Let 'y (Let 'x.1 20
                           (Let 'x.2 22
                                 '(+ x.1 x.2)))
                  'y)))

  (check-match
   (remove-complex-opera*
    (Program (Info) (If '(not (< 3 7)) 1 '(+ 4 5))))
   (Program _ (If (Let tmp '(< 3 7) `(not ,tmp)) 1 '(+ 4 5)))))