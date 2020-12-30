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
    [`(and ,a ,b) `(if ,(shrink-exp a) ,(shrink-exp b) #f)]
    [`(or ,a ,b) `(if ,(shrink-exp a) #t ,(shrink-exp b))]
    ;; Express all inequality comparisions in terms of <, using not and reversing arguments
    [`(> ,a ,b) `(< ,(shrink-exp b) ,(shrink-exp a))]
    [`(<= ,a ,b) `(not (< ,(shrink-exp b) ,(shrink-exp a)))]
    [`(>= ,a ,b) `(not (< ,(shrink-exp a) ,(shrink-exp b)))]
    ;; These cannot be shrunk any further, they are left unchanged, but non-terminals are recured upon
    [(? atom? x) x]
    [`(,op . ,args) `(,op . ,(map shrink-exp args))]
    [(LetR var val e) (LetR var (shrink-exp val) (shrink-exp e))]))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (check-equal? (shrink (Program (Info) '(> a b))) (Program (Info) '(< b a)))

  (check-equal? (shrink-exp 5) 5)
  (check-equal? (shrink-exp '(- 5)) '(- 5))
  (check-equal? (shrink-exp '(- 4 5)) '(+ 4 (- 5)))
  (check-equal? (shrink-exp '(and a (<= b c))) '(if a (not (< c b)) #f))
  (check-equal? (shrink-exp '(or c (>= d e))) '(if c #t (not (< d e))))
  (check-equal? (shrink-exp (LetR 'x 5 '(> 4 5))) (LetR 'x 5 '(< 5 4)))
  (check-equal? (shrink-exp '(if #t (- 1 2) (read))) '(if #t (+ 1 (- 2)) (read))))
