#lang typed/racket

(require typed/rackunit
         "types.rkt")

(provide select-instructions)

(define (select-instructions [c0 : C0]) : X86
  (match c0
    [`(program ,info ,p) `(program ,info ,(map tail-to-block p))]))

(define (tail-to-block [tail : (Pairof Symbol TailC0)]) : (Pairof Symbol BlockX86)
  (match tail
    [`(,label . ,tail) `(,label . (block () ,@(instructions-tail tail)))]))

(define (instructions-tail [tail : TailC0]) : (Listof InstrX86)
  (match tail
    [`(return ,r) (append (instructions-asgn '(reg rax) r) '((jmp conclusion)))]
    [`(seq ,stmt ,tail) (append (instructions-stmt stmt) (instructions-tail tail))]))

(define (instructions-stmt [stmt : StmtC0]) : (Listof InstrX86)
  (match stmt [`(assign ,x ,exp) (instructions-asgn (instructions-arg x) exp)]))

(define (instructions-asgn [x : ArgX86] [exp : ExpC0]) : (Listof InstrX86)
  (match* (x exp)
    [(x (or (? integer? v) (? symbol? v))) `((mov ,x ,(instructions-arg (cast v ArgC0))))]
    [('(reg rax) '(read)) '((call read_int))]
    [(x '(read)) `((call read_int) (mov ,x (reg rax)))]
    [(`(var ,x) (or `(+ ,x ,y) `(+ ,y ,x))) `((add (var ,x) ,(instructions-arg (cast y ArgC0))))]
    [(x `(+ ,a ,b)) `((mov ,x ,(instructions-arg a)) (add ,x ,(instructions-arg b)))]
    [(`(var ,x) `(- ,x)) `((neg (var ,x)))]
    [(x `(- ,y)) `((mov ,x ,(instructions-arg y)) (neg ,x))]))

(define (instructions-arg [arg : ArgC0]) : ArgX86
  (match arg
    [(? integer? i) `(int ,i)]
    [(? symbol? v) `(var ,v)]))



(check-equal?
 (select-instructions '(program ()
                                ((start .
                                        (seq (assign x.1 20)
                                             (seq (assign x.2 22)
                                                  (seq (assign y (+ x.1 x.2))
                                                       (return y))))))))
 '(program ()
           ((start . 
                   (block
                    ()
                    (mov (var x.1) (int 20))
                    (mov (var x.2) (int 22))
                    (mov (var y) (var x.1))
                    (add (var y) (var x.2))
                    (mov (reg rax) (var y))
                    (jmp conclusion))))))