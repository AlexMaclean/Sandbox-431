#lang typed/racket

(require "verify.rkt"
         "uniquify.rkt"
         "remove-complex-opera.rkt"
         "explicate-control.rkt"
         "uncover-locals.rkt"
         "select-instructions.rkt"
         "assign-homes.rkt"
         "patch-instructions.rkt"
         "print-x86.rkt")

(provide compile)

(define steps
  (cast
   `((verify ,verify)
     (uniquify ,uniquify)
     (remove-complex-opera* ,remove-complex-opera*)
     (explicate-control ,explicate-control)
     (uncover-locals ,uncover-locals)
     (select-instructions ,select-instructions)
     (assign-homes ,assign-homes)
     (patch-instructions ,patch-instructions)
     (print-x86 ,print-x86))
   (Listof (List Symbol (Any -> Any)))))

(define (compile [s : Sexp] [stop : Symbol]) : Any
  (apply-steps s steps stop))

(define (apply-steps [on : Any] [steps : (Listof (List Symbol (Any -> Any)))] [stop : Symbol]) : Any
  (match steps
    ['() on]
    [(cons step rst)
     (cond
       [(equal? stop (first step)) ((second step) on)]
       [else (apply-steps ((second step) on) rst stop)])]))