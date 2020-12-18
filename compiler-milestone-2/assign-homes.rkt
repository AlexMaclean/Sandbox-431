#lang typed/racket

(require typed/rackunit
         "types.rkt"
         "utilities.rkt")

(provide assign-homes)

(define (assign-homes [prog : X86]) : X86
  (match prog
    [`(program ((locals ,(? list? l))) ,bs)
     (let ([homes (get-homes (cast l (Listof Symbol)))])
       `(program ((stack-size ,(* 8 (length l))))
                 ,(map (λ ([b : (Pairof Symbol BlockX86)])
                         `(,(car b) . ,(asng-block (cdr b) homes))) bs)))]))

(define (asng-block [b : BlockX86] [homes : (Env ArgX86)]) : BlockX86
  (match b [`(block ,info . ,instrs)
            `(block ,info . ,(map (λ ([i : InstrX86]) (asgn-instr i homes)) instrs))]))

(define (asgn-instr [i : InstrX86] [homes : (Env ArgX86)]) : InstrX86
  (match i
    [`(,op ,(? list? a) ,(? list? b))
     (cast `(,op ,(asgn-arg a homes) ,(asgn-arg b homes)) InstrX86)]
    [`(,op ,(? list? a))
     (cast `(,op ,(asgn-arg a homes)) InstrX86)]
    [other other]))

(define (asgn-arg [arg : ArgX86] [homes : (Env ArgX86)]) : ArgX86
  (match arg
    [`(var ,v) (lookup v homes)]
    [other other]))

(define (get-homes [locals : (Listof Symbol)]) : (Env ArgX86)
  (map (λ ([l : Symbol] [i : Integer]) `(,l (deref rbp ,i)))
       locals (range -8 (* -8 (add1 (length locals))) -8)))


(check-equal? (assign-homes '(program ((locals (tmp.1 tmp.2)))
                                      ((start . (block ()
                                                       (mov (int 10) (var tmp.1))
                                                       (neg (var tmp.1))
                                                       (mov (var tmp.1) (var tmp.2))
                                                       (add (int 52) (var tmp.2))
                                                       (mov (var tmp.2) (reg rax)))))))
              '(program
                ((stack-size 16))
                ((start .
                        (block 
                         ()
                         (mov (int 10) (deref rbp -8))
                         (neg (deref rbp -8))
                         (mov (deref rbp -8) (deref rbp -16))
                         (add (int 52) (deref rbp -16))
                         (mov (deref rbp -16) (reg rax)))))))