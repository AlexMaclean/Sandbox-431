#lang typed/racket

(provide ExpR1 R1
         C0 TailC0 ExpC0 StmtC0 ArgC0
         X86 InstrX86 ArgX86 BlockX86
         Program Env)

;; R1 Language
(define-type R1 (Program ExpR1))
(define-type ExpR1 (U Integer (List 'read) (List '- ExpR1) (List '+ ExpR1 ExpR1)
                      Symbol (List 'let (List (List Symbol ExpR1)) ExpR1)))

;; C0 Language
(define-type C0 (Program (Listof (Pairof Symbol TailC0))))
(define-type TailC0 (U (List 'return ExpC0) (List 'seq StmtC0 TailC0)))
(define-type StmtC0 (List 'assign Symbol ExpC0))
(define-type ExpC0 (U ArgC0 (List 'read) (List '- ArgC0) (List '+ ArgC0 ArgC0)))
(define-type ArgC0 (U Integer Symbol))

;; x86 Language
(define-type X86 (Program (Listof (Pairof Symbol BlockX86))))
(define-type BlockX86 (List* 'block Any (Listof InstrX86)))
(define-type InstrX86 (U (List 'add ArgX86 ArgX86) (List 'sub ArgX86 ArgX86) (List 'mov ArgX86 ArgX86)
                         (List 'ret) (List 'neg ArgX86) (List 'call Symbol)
                         (List 'jmp Symbol) (List 'push ArgX86) (List 'pop ArgX86)))
(define-type ArgX86 (U (List 'int Integer) (List 'reg Register) (List 'deref Register Integer)
                       (List 'var Symbol)))
(define-type Register (U 'rsp 'rbp 'rax 'rbx 'rcx 'rdx 'rsi 'rdi
                         'r8 'r9 'r10 'r11 'r12 'r13 'r14 'r15))

;; Common
(define-type (Program A) (List 'program (Env Any) A))
(define-type (Env A) (Listof (List Symbol A)))