#lang typed/racket

(require "types.rkt"
         "utilities.rkt")

(provide print-x86)

(define BOILER-PLATE : String
  "section .text
extern read_int
~a
global blungentle_main 
blungentle_main:
    push rbp
    mov rbp, rsp
    sub rsp, ~a
    jmp start
conclusion:
    add rsp, ~a
    pop rbp
    ret
")


(define (print-x86 [p : X86]) : String
  (define stack-size (program-info p 'stack-size))
  (format BOILER-PLATE
          (string-join print-block (Program-body p))
          stack-size stack-size))

(define (print-block [b : (Labeled BlockX86)]) : String
  (format "~a:\n~a" (first b)
          (string-join
           (λ (i)
             (format "    ~a\n" (print-instr (cast i InstrX86)))) (list-tail (second b) 2))))

(define (print-instr [i : InstrX86]) : String
  (match i
    [(cons op args) (format "~a ~a" op (string-join-delimiter (map print-arg args) ", "))]))

(define (print-arg [a : (U ArgX86 LabelX86)]) : String
  (match a
    [`(deref ,r ,i) (format "[~a~a~a]" r (if (negative? i) "" "+") i)]
    [(or (? integer? v) `(label ,v) `(reg ,v)) (~a v)]))

(define #:∀ (A) (string-join [form : (A -> String)] [lst : (Listof A)]) : String
  (apply string-append (map form lst)))

(define (string-join-delimiter [l : (Listof String)] [d : String]) : String
  (match l
    ['() ""]
    [(list a) a]
    [(cons f r) (string-append f d (string-join-delimiter r d))]))