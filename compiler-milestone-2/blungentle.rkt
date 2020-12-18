#lang typed/racket

(require "compiler.rkt")

(define (blungentle-args [args : (Vectorof String)]) : Void
  (match args
    [(vector flag input output) (blungentle (get-flag flag) input output)]
    [(vector input output) (blungentle 'none input output)]
    [other (error "Invalid arguments")]))

(define (get-flag [flag : String]) : Symbol
  (match flag
    ["-r" 'remove-complex-opera*]
    ["-c" 'uncover-locals]
    [other (error "Unrecognized flag ~a" other)]))

(define (blungentle [flag : Symbol] [input : Path-String] [output : Path-String]) : Void
  (define exp : Sexp (with-input-from-file input (λ () (cast (read) Sexp))))
  (with-output-to-file output #:exists 'truncate
    (λ () (display (compile exp flag)))))

(module+ main
  (blungentle-args (current-command-line-arguments)))
