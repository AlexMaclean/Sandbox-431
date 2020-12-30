#lang typed/racket

(provide patch-instructions)

(require "types.rkt")

;; Iterates through each instruction in an X86 program and transforms each if it tries to interact
;; with memeory in an invalid way
(define (patch-instructions [p : X86]) : X86
  (match p [(Program info body) (Program info (map patch-block body))]))

;; Iterates through each instr in the block applying patch-instr
(define (patch-block [b : BlockX86]) : BlockX86
  (match b [(BlockX86 label info instrs) (BlockX86 label info (append-map patch-instr instrs))]))

;; Given a pseudo-x86 instruction tranlates it into a list of intructions ready to be printed
(define (patch-instr [i : InstrX86]) : (Listof InstrX86)
  (match i
    ;; Moving a register to itslef has no effect, these instructions could get created if
    ;; 2 variables use the same register or memory location
    [`(mov ,x ,x) '()]
    ;; Adding or subtracting zero is meaningless os remove instructions that do so
    [`(,(or 'add 'sub) ,dest 0) '()]
    ;; If an instruction tries to access a location in memory twice, uses rax as a intermediate
    [`(,op ,(? Deref? dst) ,(? Deref? src))
     `((mov ,(Reg 'rax) ,src)
       (,op ,dst ,(Reg 'rax)))]
    ;;
    [`(cmp ,(? integer? arg1) ,arg2)
     `((mov ,(Reg 'rax) ,arg1)
       (cmp ,(Reg 'rax) ,arg2))]
    ;; Other instructions are okay, so leave them alone
    [other (list other)]))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit "utilities.rkt")
  
  (check-equal?
   (patch-instructions
    (Program
     (Info)
     (list (BlockX86 'start '()
                     `((mov ,(Deref 'rbp -8) 42)
                       (add ,(Deref 'rbp -8) 0)
                       (mov ,(Deref 'rbp -8) ,(Deref 'rbp -8))
                       (mov ,(Deref 'rbp -16) ,(Deref 'rbp -8))
                       (mov ,(Reg 'rax) ,(Deref 'rbp -16))
                       (jmp conclusion))))))
   (Program
    (Info)
    (list (BlockX86 'start '()
                    `((mov ,(Deref 'rbp -8) 42)
                      (mov ,(Reg 'rax) ,(Deref 'rbp -8))
                      (mov ,(Deref 'rbp -16) ,(Reg 'rax))
                      (mov ,(Reg 'rax) ,(Deref 'rbp -16))
                      (jmp conclusion)))))))
