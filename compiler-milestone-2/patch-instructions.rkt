#lang typed/racket

(require typed/rackunit
         "types.rkt")

(provide patch-instructions)

(define (patch-instructions [p : X86]) : X86
  (match p [`(program ,info ,blocks) `(program ,info ,(map patch-block blocks))]))

(define (patch-block [b : (Pairof Symbol BlockX86)]) : (Pairof Symbol BlockX86)
  (match b [`(,label . (block ,info . ,instrs))
            `(,label . (block ,info . ,(append* (map patch-instr instrs))))]))

(define (patch-instr [i : InstrX86]) : (Listof InstrX86)
  (match i
    [`(,op (deref ,to1 ,to2) (deref ,frm1 ,frm2))
     (cast `((mov (reg rax) (deref ,frm1 ,frm2))
             (,op (deref ,to1 ,to2) (reg rax)))
           (Listof InstrX86))]
    [other (list other)]))


(check-equal? (patch-instructions '(program
                                    ((stack-size 16))
                                    ((start .
                                            (block 
                                             ()
                                             (mov (deref rbp -8) (int 42))
                                             (mov (deref rbp -16) (deref rbp -8))
                                             (mov (reg rax) (deref rbp -16))
                                             (jmp conclusion)
                                             )))))
              '(program
                ((stack-size 16))
                ((start .
                        (block 
                         ()
                         (mov (deref rbp -8) (int 42))
                         (mov (reg rax) (deref rbp -8))
                         (mov (deref rbp -16) (reg rax))
                         (mov (reg rax) (deref rbp -16))
                         (jmp conclusion)
                         )))))
              
