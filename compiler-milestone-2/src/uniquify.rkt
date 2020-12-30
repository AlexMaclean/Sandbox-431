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
    [(LetR x e body)
     (let ([x* (gensym x)]) (LetR x* (uniquify-exp e env) (uniquify-exp body (hash-set env x x*))))]))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit "test-utilities.rkt" "utilities.rkt")

  (check-match
   (uniquify
    (Program (Info)
             (LetR 'x (LetR 'x 4
                            '(+ x 1))
                   '(+ x #t))))
   (Program _
            (LetR x.2 (LetR x.1 4
                            `(+ ,x.1 1))
                  `(+ ,x.2 #t))))

  (check-match
   (uniquify
    (Program (Info)
             (LetR 'x 32 `(+ ,(LetR 'x 10 'x) x))))
   (Program _
            (LetR x.1 32 `(+ ,(LetR x.2 10 x.2) ,x.1)))))
