#lang typed/racket

(provide uncover-live)

(require "types.rkt")


(define (uncover-live [p : X86]) : X86
  (define blocks : (Immutable-HashTable Symbol (Listof InstrX86))
    (make-immutable-hash
     (map (λ ([b : BlockX86]) : (Pairof Symbol (Listof InstrX86))
            (cons (BlockX86-label b) (BlockX86-instrs b))) (Program-body p))))
  (define lives : (HashTable Symbol (Listof Live)) (make-hash))
  (for ([b (Program-body p)])
    (get-live-block (BlockX86-label b) blocks lives))
  (match p
    [(Program info body)
     (Program info (map (λ ([b : BlockX86])
                          (BlockX86
                           (BlockX86-label b)
                           (rest (hash-ref lives (BlockX86-label b)))
                           (BlockX86-instrs b))) body))]))

(define (get-live-block [l : Symbol]
                        [blocks : (HashTable Symbol (Listof InstrX86))]
                        [lives : (HashTable Symbol (Listof Live))]) : (Listof (Listof Symbol))
  (hash-ref! lives l
             (λ ()
               (define instrs (hash-ref blocks l))
               (uncover-live-instrs instrs
                                    (match instrs
                                      [`(,_ ... (jmp conclusion)) '()]
                                      [`(,_ ... (jmp-if ,_ ,(? symbol? l1)) (jmp ,(? symbol? l2)))
                                       (set-union (first (get-live-block l1 blocks lives))
                                                  (first (get-live-block l2 blocks lives)))]
                                      [`(,_ ... (jmp ,(? symbol? l)))
                                       (first (get-live-block l blocks lives))])))))

;; Given a list of instructions and a set of variables that are live after the last instruction is run
;; returns a list of live variables coresponding to the live after set for each instruction, as well
;; as the live before the first instruction.
(define (uncover-live-instrs [l : (Listof InstrX86)] [a : Live]) : (Listof Live)
  (foldr (λ ([i : InstrX86] [lives : (Listof (Listof Symbol))]) : (Listof (Listof Symbol))
           (define-values (r w) (vars-used-instr i))
           (cons (set-union (set-subtract (first lives) w) r) lives))
         (list a) l))

;; Given an x86 instruction, returns the variables, read and written to by that instruction
;;                                                      Read           Write
(define (vars-used-instr (a : InstrX86)) : (Values (Listof Symbol) (Listof Symbol))
  (match a
    [`(,(or 'mov 'movzx) ,dst ,src) (values (vars-used-arg src) (vars-used-arg dst))]
    [`(,(or 'add 'sub 'xor) ,dst ,arg)
     (values (set-union (vars-used-arg dst) (vars-used-arg arg)) (vars-used-arg dst))]
    [`(neg ,arg) (values (vars-used-arg arg) (vars-used-arg arg))]
    [`(cmp ,arg1 ,arg2) (values (set-union (vars-used-arg arg1) (vars-used-arg arg2)) '())]
    [other (values '() '())]))

;; Given an x86 argument, returns the variables used, represented as a list of symbols, if the arg is
;; a var, the symbol is returned, otherwise an empty list is returned.
(define (vars-used-arg [a : ArgX86]) : (Listof Symbol)
  (match a [(Var v) (list v)] [other '()]))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit "utilities.rkt")
  
  (check-equal?
   (uncover-live
    (Program (Info)
             (list (BlockX86 'start '()
                             `((mov ,(Var 'v) 1)
                               (mov ,(Var 'w) 46)
                               (mov ,(Var 'x) ,(Var 'v))
                               (add ,(Var 'x) 7)
                               (mov ,(Var 'y) ,(Var 'x))
                               (add ,(Var 'y) 4)
                               (mov ,(Var 'z) ,(Var 'x))
                               (add ,(Var 'z) ,(Var 'w))
                               (mov ,(Var 't.1) ,(Var 'y))
                               (neg ,(Var 't.1))
                               (mov ,(Reg 'rax) ,(Var 'z))
                               (add ,(Reg 'rax) ,(Var 't.1))
                               (jmp conclusion))))))
   (Program (Info)
            (list (BlockX86 'start 
                            '((v)
                              (v w)
                              (x w)
                              (x w)
                              (y w x)
                              (x y w)
                              (z w y)
                              (y z)
                              (t.1 z)
                              (z t.1)
                              (t.1)
                              ()
                              ())
                            `((mov ,(Var 'v) 1)
                              (mov ,(Var 'w) 46)
                              (mov ,(Var 'x) ,(Var 'v))
                              (add ,(Var 'x) 7)
                              (mov ,(Var 'y) ,(Var 'x))
                              (add ,(Var 'y) 4)
                              (mov ,(Var 'z) ,(Var 'x))
                              (add ,(Var 'z) ,(Var 'w))
                              (mov ,(Var 't.1) ,(Var 'y))
                              (neg ,(Var 't.1))
                              (mov ,(Reg 'rax) ,(Var 'z))
                              (add ,(Reg 'rax) ,(Var 't.1))
                              (jmp conclusion))))))

  (check-equal?
   (uncover-live
    (Program (Info) (list
                     (BlockX86 'b '()
                               `((mov ,(Reg 'rax) ,(Var 'a))
                                 (jmp conclusion)))
                     (BlockX86 'a '()
                               `((mov ,(Var 'a) 5)
                                 (jmp b))))))
   (Program (Info) (list               
                    (BlockX86 'b '(() ())
                              `((mov ,(Reg 'rax) ,(Var 'a))
                                (jmp conclusion)))
                    (BlockX86 'a '((a) (a))
                              `((mov ,(Var 'a) 5)
                                (jmp b))))))

  (check-equal?
   (uncover-live
    (Program (Info)
             (list (BlockX86 'a '()
                             `((mov ,(Var 'a) 1)
                               (mov ,(Var 'b) 2)
                               (cmp 3 4)
                               (jmp-if e b)
                               (jmp c)))
                   (BlockX86 'b '()
                             `((mov ,(Reg 'rax) ,(Var 'a))
                               (jmp conclusion)))
                   (BlockX86 'c '()
                             `((mov ,(Reg 'rax) ,(Var 'b))
                               (jmp conclusion))))))
       (Program (Info)
             (list (BlockX86 'a '((a) (a b) (b a) (a b) (b a))
                             `((mov ,(Var 'a) 1)
                               (mov ,(Var 'b) 2)
                               (cmp 3 4)
                               (jmp-if e b)
                               (jmp c)))
                   (BlockX86 'b '(() ())
                             `((mov ,(Reg 'rax) ,(Var 'a))
                               (jmp conclusion)))
                   (BlockX86 'c '(() ())
                             `((mov ,(Reg 'rax) ,(Var 'b))
                               (jmp conclusion)))))))
