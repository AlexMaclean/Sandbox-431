#lang typed/racket

(provide check-match)

(require typed/rackunit)

;; No check-match in typed/rackunit? Guess I'll do it myself
(define-syntax-rule (check-match val pat)
  (check-true (match val [pat #t][_ #f])))