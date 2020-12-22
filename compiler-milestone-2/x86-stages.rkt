#lang typed/racket

(require "types.rkt"
         "utilities.rkt")

(provide select-instructions
         patch-instructions)

;; select-instructions -------------------------------------------------------------------------------

(define (select-instructions [c0 : C]) : X86
  (match c0 [(Program info body) (Program info (map tail-to-block body))]))

(define (tail-to-block [tail : (Labeled TailC)]) : BlockX86
  (match tail [`(,label ,tail) (BlockX86 label '() (instructions-tail tail))]))

(define (instructions-tail [tail : TailC]) : (Listof InstrX86)
  (append-map (λ ([stmt : (U RetC StmtC)])
                (match stmt
                  [`(return ,r) (append (instructions-asgn '(reg rax) r) '((jmp (label conclusion))))]
                  [`(assign ,x ,exp) (instructions-asgn x exp)])) tail))

(define (instructions-asgn [x : ArgX86] [exp : ExpC]) : (Listof InstrX86)
  (match* (x exp)
    [(x (? atom? v)) `((mov ,x ,v))]
    [(x '(read)) `((call (label read_int)) (mov ,x (reg rax)))]
    [(x (or `(+ ,x ,y) `(+ ,y ,x))) `((add ,x ,(cast y ArgX86)))]
    [(x `(+ ,a ,b)) `((mov ,x ,a) (add ,x ,b))]
    [(x `(- ,x)) `((neg ,x))]
    [(x `(- ,y)) `((mov ,x ,y) (neg ,x))]))

;; patch-instructions --------------------------------------------------------------------------------

;; Iterates through each instruction in an X86 program and transforms each if it tries to interact
;; with memeory in an invalid way
(define (patch-instructions [p : X86]) : X86
  (match p [(Program info body) (Program info (map patch-block body))]))

;; Iterates through each instr in the block applying patch-instr
(define (patch-block [b : BlockX86]) : BlockX86
  (match b [(BlockX86 label info instrs) (BlockX86 label info (append-map patch-instr instrs))]))


(define (patch-instr [i : InstrX86]) : (Listof InstrX86)
  (match i
    ;; If a variable never goes live, we don't allocate a register for it, so it makes it to this pass
    ;; we could default to rax or something, but lets just remove it, this isn't a guarantee the
    ;; compiler will optimize out all unused vars, but it will at least get ride of the full dead ones
    [`(,op UNUSED ,more ...) '()]
    ;; Moving a register to itslef has no effect, these instructions could get created if
    ;; this one could happen if 2 variables use the same register or memory location
    [`(mov ,x ,x) '()]
    ;; Adding or subtracting zero is meaningless or remove instructions that do so
    [`(,(or 'add 'sub) ,dest 0) '()]
    ;; If an instruction tries to access a location in memory twice, uses rax as a intermediate
    [`(,op (deref ,to1 ,to2) (deref ,frm1 ,frm2))
     `((mov (reg rax) (deref ,frm1 ,frm2))
       (,op (deref ,to1 ,to2) (reg rax)))]
    ;; Other instructions are okay, so leave them alone
    [other (list other)]))

