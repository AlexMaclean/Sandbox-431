#lang typed/racket

(provide R1 ExpR1
         C0 TailC0 RetC0 StmtC0 ExpC0
         X86 InstrX86 ArgX86 BlockX86 LabelX86
         (struct-out Program) Env Labeled Atom)

;; R1 Language
(define-type R1 (Program ExpR1))
(define-type ExpR1 (U Atom (List 'read) (List '- ExpR1) (List '+ ExpR1 ExpR1)
                      (List 'let (List (List Symbol ExpR1)) ExpR1)))

;; C0 Language
(define-type C0 (Program (Env TailC0)))
(define-type TailC0 (U (List RetC0) (Pairof StmtC0 TailC0)))
(define-type RetC0 (List 'return ExpC0))
(define-type StmtC0 (List 'assign Symbol ExpC0))
(define-type ExpC0 (U Atom (List 'read) (List '- Atom) (List '+ Atom Atom)))

;; x86 Language
(define-type X86 (Program (Env BlockX86)))
(define-type BlockX86 (List* 'block Any (Listof InstrX86)))
(define-type InstrX86 (U (List 'add ArgX86 ArgX86) (List 'mov ArgX86 ArgX86)
                         (List 'neg ArgX86)
                         (List 'call LabelX86) (List 'jmp LabelX86)))
(define-type LabelX86 (List 'label Symbol))
(define-type ArgX86 (U Integer (List 'reg Register) (List 'deref Register Integer) Symbol))
(define-type Register (U 'rsp 'rbp 'rax 'rbx 'rcx 'rdx 'rsi 'rdi
                         'r8 'r9 'r10 'r11 'r12 'r13 'r14 'r15))

;; Common
(struct (A) Program ([info : (Env Any)] [body : A]) #:transparent)
(define-type (Env A) (Listof (Labeled A)))
(define-type (Labeled A) (List Symbol A))
(define-type Atom (U Integer Symbol))