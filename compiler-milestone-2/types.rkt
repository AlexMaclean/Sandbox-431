#lang typed/racket/base

(provide R ExpR
         C TailC RetC StmtC ExpC
         X86 (struct-out BlockX86) InstrX86 ArgX86
         (struct-out Program) (struct-out Program-Info) Env Labeled Atom)

;; R Language
(define-type R (Program ExpR))
(define-type ExpR (U Atom (List* Symbol (Listof ExpR))
                      (List 'let (List (List Symbol ExpR)) ExpR)))
;; An R expression can either be an atom, a function call: (read) | (- e) | (+ e1 e2)
;; or a let expression


;; C Language
(define-type C (Program (Env TailC)))
(define-type TailC (U (List RetC) (Pairof StmtC TailC)))
(define-type RetC (List 'return ExpC))
(define-type StmtC (List 'assign Symbol ExpC))
(define-type ExpC (U Atom (List 'read) (List '- Atom) (List '+ Atom Atom)))

;; x86 Language
(define-type X86 (Program (Listof BlockX86)))
(struct BlockX86 ([label : Symbol] [info : (Env Any)] [instrs : (Listof InstrX86)]) #:transparent)
(define-type InstrX86 (List* (U 'add 'sub 'mov 'neg 'push 'pop 'jmp 'call 'ret) (Listof ArgX86)))
(define-type ArgX86 (U Integer (List 'reg Register) (List 'deref Register Integer) Symbol
                       (List 'label Symbol)))
(define-type Register (U 'rsp 'rbp 'rax 'rbx 'rcx 'rdx 'rsi 'rdi
                         'r8 'r9 'r10 'r11 'r12 'r13 'r14 'r15))

;; Common
(struct (A) Program ([info : Program-Info] [body : A]) #:transparent)
(struct Program-Info ([locals : (Listof Symbol)]
                      [conflicts : (HashTable ArgX86 Integer)]) #:transparent)
(define-type (Env A) (Listof (Labeled A)))
(define-type (Labeled A) (List Symbol A))
(define-type Atom (U Integer Symbol))
