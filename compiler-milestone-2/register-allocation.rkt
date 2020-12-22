#lang typed/racket

(provide uncover-live
         allocate-registers
         CALLER-SAVED)

(require "types.rkt"
         "utilities.rkt")

(define CALLEE-SAVED '((reg rbx) (reg r12) (reg r13) (reg r14) (reg r15)))
(define CALLER-SAVED '((reg rdx) (reg rcx) (reg rsi) (reg rdi)
                                 (reg r8) (reg r9) (reg r10) (reg r11)))

;; uncover-live --------------------------------------------------------------------------------------

(define (uncover-live [p : X86]) : X86
  (match p [(Program info body) (Program info (map uncover-live-block body))]))

(define (uncover-live-block [b : BlockX86]) : BlockX86
  (match b [(BlockX86 label info instrs)
            (BlockX86 label `((lives ,(uncover-live-instrs instrs))) instrs)]))

(define (uncover-live-instrs [l : (Listof InstrX86)]) : (Listof (Listof Symbol))
  (foldr (λ ([i : InstrX86] [lives : (Listof (Listof Symbol))]) : (Listof (Listof Symbol))
           (define-values (r w) (vars-used-instr i))
           (cons (set-union (set-subtract (first lives) w) r) lives))
         '(()) (rest l)))

;; r w
(define (vars-used-instr (a : InstrX86)) : (Values (Listof Symbol) (Listof Symbol))
  (match a
    [`(mov ,a ,b) (values (vars-used-arg b) (vars-used-arg a))]
    [`(add ,a ,b) (values (set-union (vars-used-arg a) (vars-used-arg b)) (vars-used-arg a))]
    [`(neg ,a) (values (vars-used-arg a) (vars-used-arg a))]
    [other (values '() '())]))

(define (vars-used-arg [a : ArgX86]) : (Listof Symbol)
  (if (symbol? a) (list a) '()))


;; build-interference --------------------------------------------------------------------------------

;; allocate-registers --------------------------------------------------------------------------------

(define (allocate-registers [p : X86]) : X86
  (match p [(Program (Program-Info locs conflicts) body)
              (Program (Info #:locals locs) (do-everything body conflicts))]))

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
  (match i [`(,op . ,args)
            `(,op . ,(map (λ ([a : ArgX86]) (asgn-arg a homes)) args))]))

(define (asgn-arg [arg : ArgX86] [homes : (HashTable ArgX86 ArgX86)]) : ArgX86
  (if (symbol? arg) (hash-ref homes arg (λ () 'UNUSED)) arg))

(define (get-registers [n : Integer]) : (Listof ArgX86)
  (if (n . <= . (length CALLEE-SAVED))
      (take CALLEE-SAVED n)
      CALLEE-SAVED))

(define (get-stack [n : Integer]) : (Listof ArgX86)
  (define r (length CALLEE-SAVED))
  (if (n . > . r)
      (map (λ ([o : Integer]) `(deref rbp ,o)) (range (* -8 (add1 r)) (* -8 (add1 n)) -8))
      '()))

(define (gen-header-footer [save-regs : (Listof ArgX86)] [stack-size : Integer]) : (Listof BlockX86)
  (list
   (BlockX86 'blungentle_main '()
             `((push (reg rbp))
               (mov (reg rbp) (reg rsp))
               ,@(map (λ ([r : ArgX86]) `(push ,r)) save-regs)
               (sub (reg rsp) ,(* 8 stack-size))
               (jmp (label start))))
   (BlockX86 'conclusion '()
             `((add (reg rsp) ,(* 8 stack-size))
               ,@(map (λ ([r : ArgX86]) `(pop ,r)) (reverse save-regs))
               (pop (reg rbp))
               (ret)))))



