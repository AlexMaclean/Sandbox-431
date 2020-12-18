#lang typed/racket

(require typed/rackunit
         "types.rkt")

(provide explicate-control)

(define (explicate-control [r1 : R1]) : C0
  (match r1
    [`(program ,info ,exp) `(program ,info ((start . ,(explicate-control-tail exp))))]))

(define  (explicate-control-tail [exp : ExpR1]) : TailC0
  (match exp
    [`(let ([,x ,e]) ,body) (explicate-control-assign e x (explicate-control-tail body))]
    [other `(return ,(cast other ExpC0))]))

(define (explicate-control-assign [exp : ExpR1] [x : Symbol] [tail : TailC0]) : TailC0
  (match exp
    [`(let ([,y ,exp2]) ,body)
     (explicate-control-assign exp2 y (explicate-control-assign body x tail))]
    [other `(seq (assign ,x ,(cast other ExpC0)) ,tail)]))


(check-equal? (explicate-control '(program ()
                                           (let ([y (let ([x.1 20])
                                                      (let ([x.2 22])
                                                        (+ x.1 x.2)))])
                                             y)))
              '(program ()
                        ((start .
                                (seq (assign x.1 20)
                                     (seq (assign x.2 22)
                                          (seq (assign y (+ x.1 x.2))
                                               (return y))))))))

