#lang typed/racket

(provide uncover-locals)

(require "utilities.rkt" "types.rkt")

;; Adds a list of all the locals to the program info
(define (uncover-locals [p : C]) : C
  (match p [(Program _ body) (Program (Info #:locals (get-locals body)) body)]))

;; Given the body of a program, loops through each of the tails, and each of the statements in the
;; tails and returns a list of all the variables
(define (get-locals [p : (Env TailC)]) : (Listof Symbol)
  (append-map (λ ([method : (Labeled TailC)])
                (append-map (λ ([stmt : (U StmtC RetC IfC GotoC)])
                              (match stmt [`(assign ,x ,_) `(,x)] [other '()])) (second method))) p))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (check-equal?
   (uncover-locals (Program (Info)
                            '((start ((assign x.1 20)
                                      (assign x.2 22)
                                      (assign y (+ x.1 x.2))
                                      (return y))))))
   (Program (Program-Info '(x.1 x.2 y) '#hash())
            '((start ((assign x.1 20)
                      (assign x.2 22)
                      (assign y (+ x.1 x.2))
                      (return y)))))))