#lang typed/racket

(provide explicate-code)

(require "types.rkt")

(define-type CFG (Mutable-HashTable Symbol TailC))

;; Translates an R program into a C program
(define (explicate-code [p : R]) : C
  (define cfg : CFG (make-hash))
  (hash-set! cfg 'start (explicate-tail (Program-body p) cfg))
  (Program (Program-info p) (hash-map cfg (cast list (Symbol TailC → (Labeled TailC))))))

;; Given a expression without complex opera that needs to be translated into a C tail transfroms
;; it by either recursing down if it is a let, or returning it if it is anything else
(define (explicate-tail [exp : ExpR] [g : CFG]) : TailC
  (match exp
    [(LetR x e body) (explicate-assign e x (explicate-tail body g) g)]
    [`(if ,cnd ,thn ,els) (explicate-pred cnd (explicate-tail thn g) (explicate-tail els g) g)]
    [other `((return ,(cast other ExpC)))]))

;; Given the rest of a tail and a expressing with a variable to assign it to, prepends an assignment
;; if the expression is not a let. If it is a let, the let is expanded an prepended
(define (explicate-assign [exp : ExpR] [x : Symbol] [tail : TailC] [g : CFG]) : TailC
  (match exp
    [(LetR y exp2 body) (explicate-assign exp2 y (explicate-assign body x tail g) g)]
    [`(if ,cnd ,thn ,els) (let ([goto-rest (goto tail g)])
                            (explicate-pred cnd
                                            (explicate-assign thn x `(,goto-rest) g)
                                            (explicate-assign els x `(,goto-rest) g) g))]
    [other (cons `(assign ,x ,(cast other ExpC)) tail)]))

;; Given an expression in the predicate position of an if, and 2 tails for the then and else blocks,
;; returns a tail representing the if statement
(define (explicate-pred [exp : ExpR] [thn : TailC] [els : TailC] [g : CFG]) : TailC
  (match exp
    ;; If we have a predicate that is a boolean literal, then we don't need to preserve the
    ;; conditional, just returns the tail corresponding to the correct side of the if
    [(? boolean? b) (if b thn els)]
    ;; An eq?, > or a < is a valid part of a C program, so we'll just leave those alone
    [(or `(eq? ,a ,b) `(< ,a ,b) `(> ,a ,b)) `((if ,(cast exp CmpC) ,(goto thn g) ,(goto els g)))]
    ;; A variable can be converted to just an equality comparison with #t, the recursive call will
    ;; end up in the above match clause.
    [(? symbol? var) (explicate-pred `(eq? ,var #t) thn els g)]
    ;; When we encounter a not, we flip the clauses and reccur
    [`(not ,x) (explicate-pred x els thn g)]
    ;; Restructure an if in an if, first examining the predicate of the inner if, then going to the
    ;; apropriate predicate, with gotos to the our scoped thn and els
    [`(if ,cnd ,thn2 ,els2)
     (let ([goto-l1 (goto thn g)] [goto-l2 (goto els g)])
       (explicate-pred cnd
                       (explicate-pred thn2 `(,goto-l1) `(,goto-l2) g)
                       (explicate-pred els2 `(,goto-l1) `(,goto-l2) g) g))]
    ;; Move the assignment up a level by passing the var and val to explicate-assign, and recurr on
    ;; the body, with the same clauses
    [(LetR var val body) (explicate-assign val var (explicate-pred body thn els g) g)]))

;; Given a tail and a control flow graph, adds the tail as a new block to the graph, and returns a
;; goto instruction to that block
(define (goto [to : TailC] [g : CFG]) : GotoC
  (define label (gensym 'block))
  (hash-set! g label to)
  `(goto ,label))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit "utilities.rkt")

  (check-equal?
   (explicate-code
    (Program (Info)
             (LetR 'y (LetR 'x.1 20 (LetR 'x.2 22 '(+ x.1 x.2))) 'y)))
   (Program (Info)
            '((start ((assign x.1 20)
                      (assign x.2 22)
                      (assign y (+ x.1 x.2))
                      (return y)))))))