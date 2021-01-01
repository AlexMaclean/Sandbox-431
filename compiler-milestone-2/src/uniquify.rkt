#lang typed/racket

(provide uniquify)

(require "types.rkt")

;; Renames all the varibles with gensym ensuring uniqueness
(define (uniquify [p : R]) : R
  (match p [(Program info body) (Program info (uniquify-exp body #hash()))]))

;; Recursivly descends through the R expression replacing all var occurences with a new unique name
(define (uniquify-exp [e : ExpR] [env : (Immutable-HashTable Symbol Symbol)]) : ExpR
  (match e
    [(? symbol? var) (hash-ref env var)]
    [(or (? integer? val) (? boolean? val)) val]
    [`(,op . ,args) `(,op . ,(map (λ ([a : ExpR]) (uniquify-exp a env)) args))]
    [(If cnd thn els) (If (uniquify-exp cnd env) (uniquify-exp thn env) (uniquify-exp els env))]
    [(Let x e body)
     (let ([x* (gensym x)]) (Let x* (uniquify-exp e env) (uniquify-exp body (hash-set env x x*))))]))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit "test-utilities.rkt" "utilities.rkt")

  (check-match
   (uniquify
    (Program (Info)
             (Let 'x (Let 'x 4
                          '(+ x 1))
                  '(+ x #t))))
   (Program _
            (Let x.2 (Let x.1 4
                          `(+ ,x.1 1))
                 `(+ ,x.2 #t))))

  (check-match
   (uniquify
    (Program (Info)
             (Let 'x 32 `(+ ,(Let 'x 10 'x) x))))
   (Program _
            (Let x.1 32 `(+ ,(Let x.2 10 x.2) ,x.1))))

  (check-match
   (uniquify
    (Program (Info)
             (If #t 3 (Let 'x 32 `(+ ,(Let 'x 10 'x) x)))))
   (Program _
            (If #t 3 (Let x.1 32 `(+ ,(Let x.2 10 x.2) ,x.1))))))
