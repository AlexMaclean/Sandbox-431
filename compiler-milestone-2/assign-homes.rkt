#lang typed/racket

(require typed/rackunit
         "types.rkt"
         "utilities.rkt")

(provide assign-homes)

(define (assign-homes [prog : X86]) : X86
  (define locals (cast (program-info prog 'locals) (Listof Symbol)))
  (define homes (get-homes locals))
  (match prog [(Program _ body)
               (Program `((stack-size ,(* 8 (length locals))))
                        (map (λ ([b : (Labeled BlockX86)]) (asng-block b homes)) body))]))

(define (asng-block [b : (Labeled BlockX86)] [homes : (Env ArgX86)]) : (Labeled BlockX86)
  (match b [`(,label (block ,info . ,instrs))
            `(,label (block ,info . ,(map (λ ([i : InstrX86]) (asgn-instr i homes)) instrs)))]))

;; (map (λ ([a : (U ArgX86 LabelX86)]) (asgn-arg a homes)) 
(define (asgn-instr [i : InstrX86] [homes : (Env ArgX86)]) : InstrX86
  (match i
    [`(,op . ,args)
     (cast `(,op . ,(map (λ ([a : (U ArgX86 LabelX86)]) (asgn-arg a homes)) args)) InstrX86)]))

(define (asgn-arg [arg : (U ArgX86 LabelX86)] [homes : (Env ArgX86)]) : (U ArgX86 LabelX86)
  (if (symbol? arg) (lookup arg homes) arg))

(define (get-homes [locals : (Listof Symbol)]) : (Env ArgX86)
  (map (λ ([l : Symbol] [i : Integer]) `(,l (deref rbp ,i)))
       locals (range -8 (* -8 (add1 (length locals))) -8)))


(check-equal? (assign-homes (Program '((locals (tmp.1 tmp.2)))
                                     '((start (block ()
                                                     (mov 10 tmp.1)
                                                     (neg tmp.1)
                                                     (mov tmp.1 tmp.2)
                                                     (add 52 tmp.2)
                                                     (mov tmp.2 (reg rax)))))))
              (Program
               '((stack-size 16))
               '((start
                  (block 
                   ()
                   (mov 10 (deref rbp -8))
                   (neg (deref rbp -8))
                   (mov (deref rbp -8) (deref rbp -16))
                   (add 52 (deref rbp -16))
                   (mov (deref rbp -16) (reg rax)))))))