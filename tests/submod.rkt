#lang flit

(module m racket/base
  (provide f)
  (define (f x) x))
(require (typed-in (submod "." m)
                   [f : (Number -> Number)]))


(module m2 racket/base
  (provide g)
  (define (g x) x))
(require (typed-in 'm2
                   [g : (Number -> Number)]))


(module n flit
  (define n : Number 8))
(require (submod "." n))

(module n2 flit
  (define n2 : Number 7))
(require 'n2)

(module p flit
  (define p1 : Number -3))
(require (rename-in (submod "." p)
                    [p1 p]))

(module p2 flit
  (define p : Number -4))
(require (rename-in 'p2
                    [p p2]))

(print-only-errors #t)
(test "" (- (g (f (+ n n2))) (+ p p2)) 22)
