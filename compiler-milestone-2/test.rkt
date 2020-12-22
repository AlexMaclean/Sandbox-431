#lang typed/racket

(require typed/rackunit
         "types.rkt"
         "r1-stages.rkt"
         "c0-stages.rkt"
         "x86-stages.rkt"
         "print-x86.rkt"
         "register-allocation.rkt")


(check-equal?
 (explicate-control
  (Program '()
           '(let ([y (let ([x.1 20])
                       (let ([x.2 22])
                         (+ x.1 x.2)))])
              y)))
 (Program '()
          '((start ((assign x.1 20)
                    (assign x.2 22)
                    (assign y (+ x.1 x.2))
                    (return y))))))


(check-equal?
 (uncover-locals
  (Program '()
           '((start ((assign x.1 20)
                     (assign x.2 22)
                     (assign y (+ x.1 x.2))
                     (return y))))))
 (Program '((locals (x.1 x.2 y)))
          '((start ((assign x.1 20)
                    (assign x.2 22)
                    (assign y (+ x.1 x.2))
                    (return y))))))

(check-equal?
 (select-instructions
  (Program '()
           '((start ((assign x.1 20)
                     (assign x.2 22)
                     (assign y (+ x.1 x.2))
                     (return y))))))
 (Program '()
          (list (BlockX86 'start '()
                          '((mov x.1 20)
                            (mov x.2 22)
                            (mov y x.1)
                            (add y x.2)
                            (mov (reg rax) y)
                            (jmp (label conclusion)))))))



(check-equal?
 (patch-instructions
  (Program
   '((stack-size 16))
   (list (BlockX86 'start '()
                   '((mov (deref rbp -8) 42)
                     (mov (deref rbp -16) (deref rbp -8))
                     (mov (reg rax) (deref rbp -16))
                     (jmp (label conclusion)) )))))
 (Program
  '((stack-size 16))
  (list (BlockX86 'start '()
                  '((mov (deref rbp -8) 42)
                    (mov (reg rax) (deref rbp -8))
                    (mov (deref rbp -16) (reg rax))
                    (mov (reg rax) (deref rbp -16))
                    (jmp (label conclusion)) )))))
              


(check-equal?
 (assign-homes
  (Program '((locals (tmp.1 tmp.2)))
           (list (BlockX86 'start '()
                           '((mov 10 tmp.1)
                             (neg tmp.1)
                             (mov tmp.1 tmp.2)
                             (add 52 tmp.2)
                             (mov tmp.2 (reg rax)))))))
 (Program '((stack-size 16))
          (list (BlockX86 'start '()
                          '((mov 10 (deref rbp -8))
                            (neg (deref rbp -8))
                            (mov (deref rbp -8) (deref rbp -16))
                            (add 52 (deref rbp -16))
                            (mov (deref rbp -16) (reg rax)))))))

(check-equal?
 (uncover-live
  (Program '()
           (list (BlockX86 'start '()
                           '((mov v 1)
                             (mov w 46)
                             (mov x v)
                             (add x 7)
                             (mov y x)
                             (add y 4)
                             (mov z x)
                             (add z w)
                             (mov t.1 y)
                             (neg t.1)
                             (mov (reg rax) z)
                             (add (reg rax) t.1)
                             (jmp (label conclusion)))))))
 (Program '()
          (list (BlockX86 'start 
                          '((lives ((v)
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
                                    ())))
                          '((mov v 1)
                            (mov w 46)
                            (mov x v)
                            (add x 7)
                            (mov y x)
                            (add y 4)
                            (mov z x)
                            (add z w)
                            (mov t.1 y)
                            (neg t.1)
                            (mov (reg rax) z)
                            (add (reg rax) t.1)
                            (jmp (label conclusion)))))))
