#lang typed/racket

(provide parse)

(require "types.rkt" "utilities.rkt")

;; Given an S-expression parses it into a R AST, this is mostly the same thing but we do a little
;; bit of repackaging into structs, this pass is useful in catching any syntax errors early
(define (parse [s : Sexp]) : R
  (Program (Info) (parse-exp (match s [`(program ,_ ,exp) exp] [other other]))))

;; Given an s-expression, returns it basically unchanged, or raises an error if it is invalid
(define (parse-exp [s : Sexp]) : ExpR
  (match s
    [(? atom? x) x]
    [`(,(? nulary-op? o)) `(,o)]
    [`(,(? unary-op? o) ,x) `(,o ,(parse-exp x))]
    [`(,(? binary-op? o) ,a ,b) `(,o ,(parse-exp a) ,(parse-exp b))]
    [`(let ([,(? symbol? var) ,val]) ,e) (LetR var (parse-exp val) (parse-exp e))]
    [`(if ,cnd ,thn ,els) `(if ,(parse-exp cnd) ,(parse-exp thn) ,(parse-exp els))]
    [other (error 'Parse-Error "Invalid expression ~e" other)]))

(define-predicate nulary-op? 'read)
(define-predicate unary-op? (U '- 'not))
(define-predicate binary-op? (U '+ '- 'and 'or '> '< '>= '<= 'eq?))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (check-equal? (parse '(+ (read) (- (+ 3 5))))
                (Program (Info) '(+ (read) (- (+ 3 5)))))
  (check-equal? (parse '(program () (if #f 3 4)))
                (Program (Info) '(if #f 3 4)))
  (check-equal? (parse-exp '(let ([y 4]) (> y x)))
                (LetR 'y 4 '(> y x)))

  (check-exn #px"Parse-Error: Invalid expression" (λ () (parse-exp '(+ 4 5 6))))
  (check-exn #px"Parse-Error: Invalid expression" (λ () (parse-exp '(-))))
  (check-exn #px"Parse-Error: Invalid expression" (λ () (parse-exp '(let ([x 78]))))))
