#lang typed/racket

(provide select-instructions)

(require "types.rkt" "utilities.rkt")

;; Translates a C program into an x86 program
(define (select-instructions [p : C]) : X86
  (match p [(Program info body) (Program info (map tail-to-block body))]))

;; Translates a C tail to an x86 block
(define (tail-to-block [tail : (Labeled TailC)]) : BlockX86
  (match tail [`(,label ,tail) (BlockX86 label '() (append-map instructions-tail tail))]))

;; Given an instruction in a C tail, translates it to a list of x86 instructions
(define (instructions-tail [stmt : (U RetC StmtC GotoC IfC)]) : (Listof InstrX86)
  (match stmt
    [`(assign ,x ,exp) (instructions-asgn (to-arg-x86 x) exp)]
    [`(return ,r) `(,@(instructions-asgn (Reg 'rax) r) (jmp conclusion))]
    [`(goto ,l1) `((jmp ,l1))]
    [`(if (,cmp ,a1 ,a2) (goto ,l1) (goto ,l2)) `((cmp ,(to-arg-x86 a1) ,(to-arg-x86 a2))
                                                  (jmp-if ,(get-cc cmp) ,l1)
                                                  (jmp ,l2))]))

;; Given a expression and the value it is assigned to, translates it into x86 instructions
(define (instructions-asgn [lhs : ArgX86] [exp : ExpC]) : (Listof InstrX86)
  (match* (lhs exp)
    [(_ (? atom? v)) `((mov ,lhs ,(to-arg-x86 v)))]
    [(_ '(read)) `((call read_int) (mov ,lhs ,(Reg 'rax)))]
    [((Var x) (or `(+ ,x ,y) `(+ ,y ,x))) `((add ,lhs ,(to-arg-x86 y)))]
    [(_ `(+ ,a ,b)) `((mov ,lhs ,(to-arg-x86 a)) (add ,lhs ,(to-arg-x86 b)))]
    [((Var x) `(- ,x)) `((neg ,lhs))]
    [(_ `(- ,y)) `((mov ,lhs ,(to-arg-x86 y)) (neg ,lhs))]
    [((Var x) `(not ,x)) `((xor ,lhs 1))]
    [(_ `(not ,y)) `((mov ,lhs ,(to-arg-x86 y)) (xor ,lhs 1))]
    [(_ `(,cmp ,a ,b)) `((cmp ,(to-arg-x86 a) ,(to-arg-x86 b))
                         (set ,(get-cc cmp) ,(Reg 'al))
                         (movzx ,lhs ,(Reg 'al)))]))

;; Translates an atom which is used to represent arguments in the C language to an arg in x86
(define (to-arg-x86 [a : Atom]) : ArgX86
  (match a
    [(? symbol? s) (Var s)]
    [(? boolean? b) (if b 1 0)]
    [(? integer? i) i]))

;; Translates a comparison in the C language to the coresponding condition code in x86
(define (get-cc [op : Symbol]) : CC
  (match op ['eq? 'e] ['< 'l] ['> 'g]))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (check-equal?
   (select-instructions
    (Program (Info)
             '((start ((assign x.1 20)
                       (assign x.2 22)
                       (assign y (+ x.1 x.2))
                       (return y))))))
   (Program (Info)
            (list (BlockX86 'start '()
                            `((mov ,(Var 'x.1) 20)
                              (mov ,(Var 'x.2) 22)
                              (mov ,(Var 'y) ,(Var 'x.1))
                              (add ,(Var 'y) ,(Var 'x.2))
                              (mov ,(Reg 'rax) ,(Var 'y))
                              (jmp conclusion))))))
  (check-equal?
   (select-instructions
    (Program (Info)
             '((start ((assign x #t)
                       (goto something)))
               (something ((if (eq? 4 5) (goto a) (goto b))))
               (a ((return 4)))
               (b ((return x))))))
   (Program (Info)
            (list (BlockX86 'start '() `((mov ,(Var 'x) 1) (jmp something)))
                  (BlockX86 'something '() '((cmp 4 5) (jmp-if e a) (jmp b)))
                  (BlockX86 'a '() `((mov ,(Reg 'rax) 4) (jmp conclusion)))
                  (BlockX86 'b '() `((mov ,(Reg 'rax) ,(Var 'x)) (jmp conclusion))))))

  (check-equal? (instructions-asgn (Var 'x) '(read)) `((call read_int) (mov ,(Var 'x) ,(Reg 'rax))))
  (check-equal? (instructions-asgn (Var 'x) '(not x)) `((xor ,(Var 'x) 1)))
  (check-equal? (instructions-asgn (Var 'x) '(not #f)) `((mov ,(Var 'x) 0) (xor ,(Var 'x) 1)))
  (check-equal? (instructions-asgn (Var 'x) '(- x)) `((neg ,(Var 'x))))
  (check-equal? (instructions-asgn (Var 'x) '(- 4)) `((mov ,(Var 'x) 4) (neg ,(Var 'x))))
  (check-equal? (instructions-asgn (Var 'x) '(< z 4)) `((cmp ,(Var 'z) 4)
                                                        (set l ,(Reg 'al))
                                                        (movzx ,(Var 'x) ,(Reg 'al))))

  )
