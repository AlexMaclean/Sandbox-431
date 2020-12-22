#lang typed/racket/base

(provide (all-defined-out))

(require "types.rkt")

(define-predicate atom? Atom)

(define (Info #:locals [locals : (Listof Symbol) '()]
              #:conflicts [conflicts : (HashTable ArgX86 Integer) (make-hash)]) : Program-Info
  (Program-Info locals conflicts))
