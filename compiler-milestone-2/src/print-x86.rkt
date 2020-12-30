#lang typed/racket

(provide print-x86)

(require "types.rkt")

(define HEADER : String
  "section .text
extern read_int
global blungentle_main
")

(define INDENT : String "\n    ")


(define (print-x86 [p : X86]) : String
  (string-join (map print-block (Program-body p)) #:before-first HEADER))

(define (print-block [b : BlockX86]) : String
  (string-join (map print-instr (BlockX86-instrs b)) INDENT
               #:before-first (format "~a:~a" (BlockX86-label b) INDENT)
               #:after-last "\n"))

(define (print-instr [i : InstrX86]) : String
  (match i
    [`(jmp-if ,cc ,l) (format "j~a ~a" cc (print-arg l))]
    [`(set ,cc ,a) (format "set~a ~a" cc (print-arg a))]
    [`(,cmd . ,rst) (format "~a ~a" cmd (string-join (map print-arg rst) ", "))]))

(define (print-arg [a : ArgX86]) : String
  (match a
    [(Deref r i) (format "[~a~a~a]" r (if (negative? i) "" "+") i)]
    [(or (? integer? v) (? symbol? v) (Reg v)) (~a v)]
    [(Var v) "rax"]))
