#lang typed/racket

(require typed/rackunit
         "types.rkt")

(provide explicate-control)

(define (explicate-control [r1 : R1]) : C0
  (match r1 [(Program info body) (Program info `((start ,(explicate-control-tail body))))]))

(define  (explicate-control-tail [exp : ExpR1]) : TailC0
  (match exp
    [`(let ([,x ,e]) ,body) (explicate-control-assign e x (explicate-control-tail body))]
    [other `((return ,(cast other ExpC0)))]))

(define (explicate-control-assign [exp : ExpR1] [x : Symbol] [tail : TailC0]) : TailC0
  (match exp
    [`(let ([,y ,exp2]) ,body)
     (explicate-control-assign exp2 y (explicate-control-assign body x tail))]
    [other (cons `(assign ,x ,(cast other ExpC0)) tail)]))


(check-equal? (explicate-control (Program '()
                                          '(let ([y (let ([x.1 20])
                                                      (let ([x.2 22])
                                                        (+ x.1 x.2)))])
                                             y)))
              (Program '()
                       '((start ((assign x.1 20)
                                 (assign x.2 22)
                                 (assign y (+ x.1 x.2))
                                 (return y))))))

