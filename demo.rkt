#lang s-exp "main.rkt"

(define-type Foo
  [Bar (x : Number)]
  [Baz (y : Boolean)])

(define y (Bar 3))

(type-case Foo (Bar 3)
           [(Bar z) #t]
           [(Baz z) #f])

(define (map (f : ('a -> 'b)) (lst : (Listof 'a))) : (Listof 'b)
  TODO)

(define (f [x : 'b] [y : Foo]) : Number
  (+ (if (type-case Foo y
           [(Bar z) #t]
           [(Baz z) TODO]) 3 (TODO x y #t)) TODO))
