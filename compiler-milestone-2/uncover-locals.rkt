#lang typed/racket

(require typed/rackunit
         "types.rkt")

(provide uncover-locals)

(define (uncover-locals [c0 : C0]) : C0
  (match c0
    [`(program ,_ ,b) `(program ((locals ,(get-locals b))) ,b)]))

(define (get-locals [p : (Listof (Pairof Symbol TailC0))]) : (Listof Symbol)
  (append-map (λ ([method : (Pairof Symbol TailC0)]) (get-locals-tail (cdr method))) p))

(define (get-locals-tail [t : TailC0]) : (Listof Symbol)
  (match t
    [`(seq (assign ,x ,_) ,rest) (cons x (get-locals-tail rest))]
    [other '()]))


(check-equal?
 (uncover-locals '(program ()
                           ((start .
                                   (seq (assign x.1 20)
                                        (seq (assign x.2 22)
                                             (seq (assign y (+ x.1 x.2))
                                                  (return y))))))))
 '(program ((locals (x.1 x.2 y)))
           ((start .
                   (seq (assign x.1 20)
                        (seq (assign x.2 22)
                             (seq (assign y (+ x.1 x.2))
                                  (return y))))))))