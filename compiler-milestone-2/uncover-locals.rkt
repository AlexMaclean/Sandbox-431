#lang typed/racket

(require typed/rackunit
         "types.rkt")

(provide uncover-locals)

(define (uncover-locals [c0 : C0]) : C0
  (match c0 [(Program _ body) (Program `((locals ,(get-locals body))) body)]))

(define (get-locals [p : (Env TailC0)]) : (Listof Symbol)
  (append-map (λ ([method : (Labeled TailC0)]) (get-locals-tail (second method))) p))

(define (get-locals-tail [t : TailC0]) : (Listof Symbol)
  (match t
    [(cons `(assign ,x ,_) rest) (cons x (get-locals-tail rest))]
    [other '()]))


(check-equal?
 (uncover-locals (Program '()
                          '((start ((assign x.1 20)
                                    (assign x.2 22)
                                    (assign y (+ x.1 x.2))
                                    (return y))))))
 (Program '((locals (x.1 x.2 y)))
          '((start ((assign x.1 20)
                    (assign x.2 22)
                    (assign y (+ x.1 x.2))
                    (return y))))))