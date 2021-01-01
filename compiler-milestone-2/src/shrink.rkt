#lang typed/racket

(provide shrink)

(require "types.rkt" "utilities.rkt")

;; Given a program in the R language, shirinks the number of operations it could be using by expanding
;; some operations into compositions of other operations
(define (shrink [p : R]) : R
  (match p [(Program info exp) (Program info (shrink-exp exp))]))

;; Given an expression, returns an equivalent expression, but possibly changed to remove some
;; operations
(define (shrink-exp [e : ExpR]) : ExpR
  (match e
    ;; Shrink subtaction to addition of the negation of the second argument
    [`(- ,a ,b) `(+ ,(shrink-exp a) (- ,(shrink-exp b)))]
    ;; Shrink or & and by converting them to equivalent if statements, similar racket
    [`(and ,a ,b) (If (shrink-exp a) (shrink-exp b) #f)]
    [`(or ,a ,b) (If (shrink-exp a) #t (shrink-exp b))]
    ;; Express all inequality comparisions in terms of < or >, using not
    [`(<= ,a ,b) `(not (> ,(shrink-exp a) ,(shrink-exp b)))]
    [`(>= ,a ,b) `(not (< ,(shrink-exp a) ,(shrink-exp b)))]
    ;; These cannot be shrunk any further, they are left unchanged, but non-terminals are recured upon
    [(? atom? x) x]
    [`(,op . ,args) `(,op . ,(map shrink-exp args))]
    [(Let var val e) (Let var (shrink-exp val) (shrink-exp e))]
    [(If cnd thn els) (If (shrink-exp cnd) (shrink-exp thn) (shrink-exp els))]))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (check-equal? (shrink (Program (Info) '(< b a))) (Program (Info) '(< b a)))

  (check-equal? (shrink-exp 5) 5)
  (check-equal? (shrink-exp '(- 5)) '(- 5))
  (check-equal? (shrink-exp '(- 4 5)) '(+ 4 (- 5)))
  (check-equal? (shrink-exp '(and a (<= b c))) (If 'a '(not (> b c)) #f))
  (check-equal? (shrink-exp '(or c (>= d e))) (If 'c #t '(not (< d e))))
  (check-equal? (shrink-exp (Let 'x 5 '(< 5 4))) (Let 'x 5 '(< 5 4)))
  (check-equal? (shrink-exp (If #t '(- 1 2) '(read))) (If #t '(+ 1 (- 2)) '(read))))
