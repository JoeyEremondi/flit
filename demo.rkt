#lang s-exp "main.rkt"

(define-type Foo
  [Bar (x : Number)]
  [Baz (y : Boolean)])

;(define (map (f : ('a -> 'b)) (lst : (Listof 'a))) : (Listof 'b)
;  (TODO))

(define (f [x : 'b] [y : Foo]) : Number
  (+ (if (type-case Foo y
           [(Bar z) #t]
           [(Baz z) TODO]) 3 (TODO x y #t)) TODO))

;(define zz : Boolean TODO)

;(TODO "write more code!")

;(let ((x 1) (y 2)) (TODO/bindings))

;(with-command 444)

;; The entire definition containing the TODO is highlighted as a thing to be done now.
;(define/todos incomplete-fun
;  (lambda (y)
;    (inner-TODO "This function is incomplete")))
