#lang racket/base

(provide compile)

(require racket/match
         "parse.rkt"
         "type-check.rkt"
         "shrink.rkt"
         "uniquify.rkt"
         "remove-complex-opera.rkt"
         "explicate-code.rkt"
         "uncover-locals.rkt"
         "select-instructions.rkt"
         "uncover-live.rkt"
         "build-interference.rkt"
         "allocate-registers.rkt"
         "patch-instructions.rkt"
         "print-x86.rkt")

(define steps
  `((parse ,parse)
    (type-check ,type-check)
    (shrink ,shrink)
    (uniquify ,uniquify)
    (remove-complex-opera* ,remove-complex-opera*)
    (explicate-code ,explicate-code)
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