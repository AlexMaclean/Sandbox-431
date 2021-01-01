#lang typed/racket/base

(provide (all-defined-out))

;; R Language
(define-type R (Program ExpR))
(define-type ExpR (U Atom (List* Symbol (Listof ExpR)) Let If))
(struct Let ([var : Symbol] [val : ExpR] [body : ExpR]) #:prefab)
(struct If ([cnd : ExpR] [thn : ExpR] [els : ExpR]) #:prefab)

;; C Language
(define-type C (Program (Env TailC)))
(define-type TailC (U (List RetC) (List GotoC) (List IfC) (Pairof StmtC TailC)))
(define-type RetC (List 'return ExpC))
(define-type StmtC (List 'assign Symbol ExpC))
(define-type GotoC (List 'goto Symbol))
(define-type IfC (List 'if CmpC GotoC GotoC))
(define-type ExpC (U Atom (List 'read) (List '- Atom) (List '+ Atom Atom) (List 'not Atom) CmpC))
(define-type CmpC (List (U 'eq? '< '>) Atom Atom))

;; x86 Language
(define-type X86 (Program (Listof BlockX86)))
(struct BlockX86 ([label : Symbol] [lives : (Listof Live)] [instrs : (Listof InstrX86)]) #:prefab)
(define-type Live (Listof Symbol))
(define-type InstrX86 (List* (U 'add 'sub 'mov 'neg 'push 'pop 'jmp 'xor
                                'jmp-if 'call 'ret 'cmp 'set 'movzx) (Listof ArgX86)))
(define-type ArgX86 (U Integer Symbol Reg Deref Var CC))
(struct Reg ([r : Register]) #:prefab)
(struct Deref ([r : Register] [offset : Integer]) #:prefab)
(struct Var ([v : Symbol]) #:prefab)
(define-type CC (U 'e 'l 'le 'g 'ge))
(define-type Register (U 'rsp 'rbp 'rax 'rbx 'rcx 'rdx 'rsi 'rdi
                         'r8 'r9 'r10 'r11 'r12 'r13 'r14 'r15 'al))

;; Common
(struct (A) Program ([info : Program-Info] [body : A]) #:prefab)
(struct Program-Info ([locals : (Listof Symbol)]
                      [conflicts : (HashTable (U Reg Symbol) Integer)]) #:prefab)
(define-type (Env A) (Listof (Labeled A)))
(define-type (Labeled A) (List Symbol A))
(define-type Atom (U Integer Symbol Boolean))
