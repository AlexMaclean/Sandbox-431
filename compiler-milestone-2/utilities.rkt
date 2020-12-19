#lang typed/racket

(require "types.rkt")

(provide make-env lookup program-info atom?)

(define #:∀ (A) (make-env [id : Symbol] [v : A] [env : (Env A)]) : (Env A)
  (cons (list id v) env))

(define #:∀ (A) (lookup [id : Symbol] [env : (Env A)]) : A
  (define binding (findf (λ ([b : (List Symbol A)]) (equal? (first b) id)) env))
  (if binding (second binding) (error "Unbound identifier ~e" id)))

(define (program-info [p : (Program Any)] [id : Symbol]) : Any
  (lookup id (Program-info p)))

(define (atom? [v : Any]) : Boolean
  (or (symbol? v) (exact-integer? v)))

