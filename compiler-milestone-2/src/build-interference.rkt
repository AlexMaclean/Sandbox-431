#lang typed/racket

(provide build-interference)

(require "types.rkt"
         (only-in "allocate-registers.rkt" CALLER-SAVED))

(module wrapper racket
  (provide graph? unweighted-graph/undirected add-edge! add-vertex! coloring/brelaz get-edges)
  (require (prefix-in g: graph))
  (struct graph [g]) 
  
  (define (unweighted-graph/undirected es) (graph (g:unweighted-graph/undirected es))) 
  (define (add-edge! g a b) (g:add-edge! (graph-g g) a b))
  (define (add-vertex! g v) (g:add-vertex! (graph-g g) v))
  (define (coloring/brelaz g) (g:coloring/brelaz (graph-g g)))
  (define (get-edges g) (g:get-edges (graph-g g))))

;; The types here are more restrictive then the types the generic graph library can handle
(require/typed 'wrapper
               [#:opaque Graph graph?]
               [unweighted-graph/undirected ((Listof Edge) → Graph)]
               [add-edge! (Graph Vertex Vertex → Void)]
               [add-vertex! (Graph Vertex → Void)]
               [coloring/brelaz (Graph → (HashTable Vertex Integer))]
               [get-edges (Graph → (Listof Edge))])

;; Types representing the values in the conflict graph
(define-type Vertex (U Reg Symbol))
(define-type Edge (Listof Vertex))

;; ---------------------------------------------------------------------------------------------------

;; Given a program with live variables computed, builds and colors a conflict graph, and puts the
;; coloring into the program info
(define (build-interference [p : X86]) : X86
  (match p [(Program (Program-Info locs _) body)
            (Program (Program-Info locs (coloring/brelaz (get-conflicts body locs))) body)]))

;; Computes the conflict graph by iterating over the blocks, and the instructions in them and adding
;; all the live variables as verticies and all the conflicts as edges
(define (get-conflicts [body : (Listof BlockX86)] [locals : (Listof Symbol)]) : Graph
  (define conflicts : Graph (unweighted-graph/undirected (combinations CALLER-SAVED 2)))
  (for ([v locals]) (add-vertex! conflicts v))
  (for ([block body])
    (for ([instr (BlockX86-instrs block)]
          [lives (BlockX86-lives block)])
      (add-edges! conflicts (instr-conflicts instr lives))))
  conflicts)

;; Given an instruction computes the conflicts between variables in that instructions, this is the
;; edges between any variable written to, and any currently live variable
(define (instr-conflicts [i : InstrX86] [live-after : (Listof Symbol)]) : (Listof Edge)
  (match i
    [`(,(or 'mov 'movzx) ,(Var dst) ,(Var src)) (edges-to-each dst (remove src live-after))]
    [`(,(or 'mov 'movzx 'add 'sub 'xor) ,(Var dst) ,_) (edges-to-each dst live-after)]
    [(or `(set ,_ ,(Var dst)) `(neg ,(Var dst))) (edges-to-each (cast dst Symbol) live-after)]
    [`(call ,_) (cartesian-product CALLER-SAVED live-after)]
    [other '()]))

;; Returns a list of edges that go from dst to each of the elements in the live after set, excluding
;; the possible edge from dst to dst
(define (edges-to-each [dst : Symbol] [live-after : (Listof Symbol)]) : (Listof Edge)
  (filter not-loop? (map (λ ([v : Symbol]) `(,dst ,v)) live-after)))

;; Given an edge, represented as a list of 2 values, determines if it is not a looping edge
(define (not-loop? [e : Edge]) : Boolean
  (not (equal? (first e) (second e))))

;; Given a list of edges, adds all to a given graph
(define (add-edges! [g : Graph] [edges : (Listof Edge)]) : Void
  (for ([e edges])
    (add-edge! g (first e) (second e))))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (check-equal? (instr-conflicts '(call read_int) '(a))
                '((#s(Reg rdx) a) (#s(Reg rcx) a) (#s(Reg rsi) a)
                                  (#s(Reg rdi) a) (#s(Reg r8) a) (#s(Reg r9) a)
                                  (#s(Reg r10) a) (#s(Reg r11) a)))

  (check-equal? (instr-conflicts `(mov ,(Var 'v) 1) '(v)) '())
  (check-equal? (instr-conflicts `(mov ,(Var 'w) 46) '(v w)) '((w v)))
  (check-equal? (instr-conflicts `(mov ,(Var 'x) ,(Var 'v)) '(w x)) '((x w)))
  (check-equal? (instr-conflicts `(add ,(Var 'x) 7) '(w x)) '((x w)))
  (check-equal? (instr-conflicts `(mov ,(Var 'y) ,(Var 'x)) '(w x y)) '((y w)))
  (check-equal? (instr-conflicts `(add ,(Var 'y) 4) '(w x y)) '((y w) (y x)))
  (check-equal? (instr-conflicts `(mov ,(Var 'z) ,(Var 'x)) '(w y z)) '((z w) (z y)))
  (check-equal? (instr-conflicts `(add ,(Var 'z) ,(Var 'w)) '(y z)) '((z y)))
  (check-equal? (instr-conflicts `(mov ,(Var 't.1) ,(Var 'y)) '(z t.1)) '((t.1 z)))
  (check-equal? (instr-conflicts `(neg ,(Var 't.1)) '(z t.1)) '((t.1 z)))
  (check-equal? (instr-conflicts `(mov ,(Reg 'rax) ,(Var 'z)) '(t.1)) '())
  (check-equal? (instr-conflicts `(add ,(Reg 'rax) ,(Var 't.1)) '()) '())
  (check-equal? (instr-conflicts `(jmp conclusion) '()) '())

  (check-equal?
   (filter
    (λ ([p : Edge]) (not (Reg? (first p))))
    (get-edges
     (get-conflicts
      (list
       (BlockX86 'something
                 '((v) (v w) (w x) (w x) (w x y) (w x y) (w y z) (y z) (z t.1) (z t.1) (t.1) () ())
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
                   (jmp conclusion))))
      '(x y z v w t.1))))
   '((y x) (y w) (y z) (v w) (x y) (x w) (z y) (z t.1) (z w) (t.1 z) (w v) (w x) (w y) (w z)))

  (check-equal?
   (build-interference
    (Program
     (Program-Info '() #hash())
     (list
      (BlockX86 'something
                '((v) (v w) (w x) (w x) (w x y) (w x y) (w y z) (y z) (z t.1) (z t.1) (t.1) () ())
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
   (Program
    (Program-Info
     '()
     (make-hash '((w . 0) (#s(Reg r10) . 7) (#s(Reg r8) . 6) (#s(Reg r9) . 5) (t.1 . 0)
                          (#s(Reg rdi) . 4) (z . 2) (#s(Reg r11) . 3) (#s(Reg rcx) . 2)
                          (#s(Reg rdx) . 1) (x . 2) (v . 1)  (#s(Reg rsi) . 0) (y . 1))))
    (list
     (BlockX86 'something
               '((v) (v w) (w x) (w x) (w x y) (w x y) (w y z) (y z) (z t.1) (z t.1) (t.1) () ())
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
                 (jmp conclusion)))))))


