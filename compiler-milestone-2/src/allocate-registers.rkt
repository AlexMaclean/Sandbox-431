#lang typed/racket

(provide allocate-registers
         CALLER-SAVED)

(require "types.rkt"
         "utilities.rkt")

(define CALLEE-SAVED : (Listof Reg) (list (Reg 'rbx) (Reg 'r12) (Reg 'r13) (Reg 'r14) (Reg 'r15)))
(define CALLER-SAVED : (Listof Reg) (list (Reg 'rdx) (Reg 'rcx) (Reg 'rsi) (Reg  'rdi)
                                          (Reg 'r8) (Reg 'r9) (Reg 'r10) (Reg 'r11)))


;; allocate-registers --------------------------------------------------------------------------------

(define (allocate-registers [p : X86]) : X86
  (match p [(Program (Program-Info locs conflicts) body)
            (Program (Info #:locals locs)
                     (do-everything body (cast conflicts (HashTable ArgX86 Integer))))]))

(define (do-everything [body : (Listof BlockX86)]
                       [coloring : (HashTable ArgX86 Integer)]) : (Listof BlockX86)
  (define caller-colors : (Listof Integer)
    (map (λ ([reg : ArgX86]) (hash-ref coloring reg)) CALLER-SAVED))
  (define unassigned-colors  : (Listof Integer)
    (remove* caller-colors (range (add1 (apply max (hash-values coloring))))))
  (define callee-locs : (Listof ArgX86) (get-registers (length unassigned-colors)))
  (define spilled-locs : (Listof ArgX86) (get-stack (length unassigned-colors)))

  (define locs (make-immutable-hash (map (inst cons Integer ArgX86)
                                         (append caller-colors unassigned-colors)
                                         (append CALLER-SAVED callee-locs spilled-locs))))
  (define homes : (HashTable ArgX86 ArgX86)
    (make-immutable-hash (hash-map coloring (λ ([var : ArgX86] [color : Integer])
                                              (cons var (hash-ref locs color))))))
  (append (map (λ ([b : BlockX86]) (asng-block b homes)) body)
          (gen-header-footer callee-locs (length spilled-locs))))

(define (asng-block [b : BlockX86] [homes : (HashTable ArgX86 ArgX86)]) : BlockX86
  (match b [(BlockX86 label info instrs)
            (BlockX86 label info (map (λ ([i : InstrX86]) (asgn-instr i homes)) instrs))]))

(define (asgn-instr [i : InstrX86] [homes : (HashTable ArgX86 ArgX86)]) : InstrX86
  (match i [`(,op . ,args) `(,op . ,(map (λ ([a : ArgX86]) (asgn-arg a homes)) args))]))

(define (asgn-arg [arg : ArgX86] [homes : (HashTable ArgX86 ArgX86)]) : ArgX86
  (match arg [(Var v) (hash-ref homes v (λ () (Var 'UNUSED)))] [_ arg]))

(define (get-registers [n : Integer]) : (Listof ArgX86)
  (if (n . <= . (length CALLEE-SAVED))
      (take CALLEE-SAVED n)
      CALLEE-SAVED))

(define (get-stack [n : Integer]) : (Listof ArgX86)
  (define r (length CALLEE-SAVED))
  (if (n . > . r)
      (map (λ ([o : Integer]) (Deref 'rbp o)) (range (* -8 (add1 r)) (* -8 (add1 n)) -8))
      '()))

(define (gen-header-footer [save-regs : (Listof ArgX86)] [spilled-locs : Integer]) : (Listof BlockX86)
  (define stack-size : Integer (- (* (ceiling (/ (+ (length save-regs) spilled-locs) 2)) 16)
                                  (* (length save-regs) 8)))
  (list (BlockX86 'blungentle_main '()
                  `((push ,(Reg 'rbp))
                    (mov ,(Reg 'rbp) ,(Reg 'rsp))
                    ,@(map (λ ([r : ArgX86]) `(push ,r)) save-regs)
                    (sub ,(Reg 'rsp) ,stack-size)
                    (jmp start)))
        (BlockX86 'conclusion '()
                  `((add ,(Reg 'rsp) ,stack-size)
                    ,@(map (λ ([r : ArgX86]) `(pop ,r)) (reverse save-regs))
                    (pop ,(Reg 'rbp))
                    (ret)))))



