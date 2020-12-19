#lang typed/racket

(require typed/rackunit
         "types.rkt"
         "utilities.rkt")

(provide select-instructions)

(define (select-instructions [c0 : C0]) : X86
  (match c0 [(Program info body) (Program info (map tail-to-block body))]))

(define (tail-to-block [tail : (Labeled TailC0)]) : (Labeled BlockX86)
  (match tail
    [`(,label ,tail) `(,label (block () ,@(instructions-tail tail)))]))

(define (instructions-tail [tail : TailC0]) : (Listof InstrX86)
  (append*
   (map (λ ([stmt : (U RetC0 StmtC0)]) : (Listof InstrX86)
          (match stmt
            [`(return ,r) (append (instructions-asgn '(reg rax) r) '((jmp (label conclusion))))]
            [`(assign ,x ,exp) (instructions-asgn x exp)])) tail)))

(define (instructions-asgn [x : ArgX86] [exp : ExpC0]) : (Listof InstrX86)
  (match* (x exp)
    [(`(reg ,r) (? atom? v)) `((mov (reg ,r) ,(cast v Atom)))]
    [(x (? atom? v)) `((mov (reg rax) ,(cast v Atom)) (mov ,x (reg rax)))]
    [('(reg rax) '(read)) '((call (label read_int)))]
    [(x '(read)) `((call (label read_int)) (mov ,x (reg rax)))]
    [(`(reg ,r) `(+ ,a ,b)) `((mov (reg ,r) ,a) (add (reg ,r) ,b))]
    [(x `(+ ,a ,b)) `((mov (reg rax) ,a) (add (reg rax) ,b) (mov ,x (reg rax)))]
    [(`(reg ,r) `(- ,x)) `((mov (reg ,r) ,x) (neg (reg ,r)))]
    [(x `(- ,y)) `((mov (reg rax) ,y) (neg (reg rax)) (mov ,x (reg rax)))]))


(check-equal?
 (select-instructions (Program '()
                               '((start ((assign x.1 20)
                                         (assign x.2 22)
                                         (assign y (+ x.1 x.2))
                                         (return y))))))
 (Program '()
          '((start (block ()
                          (mov (reg rax) 20)
                          (mov x.1 (reg rax))
                          (mov (reg rax) 22)
                          (mov x.2 (reg rax))
                          (mov (reg rax) x.1)
                          (add (reg rax) x.2)
                          (mov y (reg rax))
                          (mov (reg rax) y)
                          (jmp (label conclusion)))))))
