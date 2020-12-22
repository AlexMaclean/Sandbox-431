#lang typed/racket

(provide explicate-control
         uncover-locals)

(require "utilities.rkt"
         "types.rkt")

;; explicate-control ---------------------------------------------------------------------------------

;; Translates an R program into a C program
(define (explicate-control [p : R]) : C
  (match p [(Program info body) (Program info `((start ,(explicate-control-tail body))))]))

;; Given a expression without complex opera that needs to be translated into a C tail transfroms
;; it by either recursing down if it is a let, or returning it if it is anything else
(define (explicate-control-tail [exp : ExpR]) : TailC
  (match exp
    [`(let ([,x ,e]) ,body) (explicate-control-assign e x (explicate-control-tail body))]
    [other `((return ,(cast other ExpC)))]))

;; Given the rest of a tail and a expressing with a variable to assign it to, prepends an assignment
;; if the expression is not a let. If it is a let, the let is expanded an prepended
(define (explicate-control-assign [exp : ExpR] [x : Symbol] [tail : TailC]) : TailC
  (match exp
    [`(let ([,y ,exp2]) ,body)
     (explicate-control-assign exp2 y (explicate-control-assign body x tail))]
    [other (cons `(assign ,x ,(cast other ExpC)) tail)]))


;; uncover-locals ------------------------------------------------------------------------------------

;; Adds a list of all the locals to the program info
(define (uncover-locals [p : C]) : C
  (match p [(Program _ body) (Program (Info #:locals (get-locals body)) body)]))

;; Given the body of a program, loops through each of the tails, and each of the statements in the
;; tails and returns a list of all the variables
(define (get-locals [p : (Env TailC)]) : (Listof Symbol)
  (append-map (λ ([method : (Labeled TailC)])
                (append-map (λ ([stmt : (U StmtC RetC)])
                              (match stmt [`(assign ,x ,_) `(,x)] [other '()])) (second method))) p))