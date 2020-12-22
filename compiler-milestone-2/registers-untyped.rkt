#lang racket

(provide build-interference)

(require graph
         "types.rkt"
         (only-in "register-allocation.rkt" CALLER-SAVED))


(define (build-interference p)
  (match p [(Program (Program-Info locs _) body)
            (Program (Program-Info locs (get-conflicts body)) body)]))

(define (get-conflicts body)
  (define conflicts (unweighted-graph/undirected (combinations CALLER-SAVED 2)))
  (for ([block body])
    (for ([instr (BlockX86-instrs block)]
          [lives (second (first (BlockX86-info block)))])
      (for ([v lives]) (add-vertex! conflicts v))
      (add-edges! conflicts (instr-conflicts instr lives))))
  (coloring/brelaz conflicts))

(define (instr-conflicts i live-after)
  (match i
    [`(add ,(? symbol? d) ,s) (filter not-loop? (map (λ (v) `(,d ,v)) live-after))]
    [`(call ,_) (cartesian-product CALLER-SAVED live-after)]
    [`(mov ,(? symbol? d) ,s) (filter not-loop? (map (λ (v) `(,d ,v)) (remove s live-after)))]
    [other '()]))

(define (not-loop? e)
  (not (equal? (first e) (second e))))

(define (add-edges! g edges)
  (for ([e edges])
    (add-edge! g (first e) (second e))))

