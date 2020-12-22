#lang racket/base

(provide compile)

(require racket/match
         "r1-stages.rkt"
         "c0-stages.rkt"
         "register-allocation.rkt"
         "registers-untyped.rkt"
         "x86-stages.rkt"
         "print-x86.rkt")

(define steps
   `((parse ,parse)
     (uniquify ,uniquify)
     (remove-complex-opera* ,remove-complex-opera*)
     (explicate-control ,explicate-control)
     (uncover-locals ,uncover-locals)
     (select-instructions ,select-instructions)
     (uncover-live ,uncover-live)
     (build-interference ,build-interference)
     (allocate-registers ,allocate-registers)
     (patch-instructions ,patch-instructions)
     (print-x86 ,print-x86)))

(define (compile sexp stop-symbol)
  (apply-steps sexp steps stop-symbol))

(define (apply-steps on steps stop-symbol)
  (match steps
    ['() on]
    [(cons step rst)
     (cond
       [(equal? stop-symbol (car step)) ((cadr step) on)]
       [else (apply-steps ((cadr step) on) rst stop-symbol)])]))

(module+ test
  (require rackunit)

  (check-equal? #t #f))