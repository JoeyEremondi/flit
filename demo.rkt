#lang s-exp "main.rkt"

;; (define-type Foo
;;   [Bar (x : Number)]
;;   [Baz (y : Boolean)])
;; 
;; (define y (Bar 3))
;; 
;; (type-case Foo (Bar 3)
;;            [(Bar z) #t]
;;            [(Baz z) #t])
;; 
;; (define (f [x : Number] [y : Boolean] [z : (Number -> Number)]) : Boolean
;;   TODO)
;; 
;; 
;; empty
;; (empty)

;; (define (e? l)
;;   (type-case (Listof 'a) l
;;     [(empty) #t]
;;     [(cons h t) #f]))

  (define (foo [xs : 'var]
                  )
          : Number
    TODO)

(define (map [f : ('a -> 'b)]
             [xs : (Listof 'a)]) : (Listof 'b)
  (type-case (Listof 'a) xs
    [empty empty]
    [(cons h t) (cons (f h) (map f t))]))

;; (define (testfun [x : String])
;;   (let* ([somevar 3])
;;     TODO))
;; 

;; (define (bad-poly [x : 'a])
;;   (+ x 3))

(define (my-length l)
  (type-case (Listof 'a) l
    [empty 0]
    [(cons a b) (+ 1 (my-length b))]))
(length '(1 2 3))
(length '(a b))

#| (define (map (f : ('a -> 'b)) (lst : (Listof 'a))) : (Listof 'b)
     TODO)

   (define (f [x : 'b] [y : Foo]) : Number
     (+ (if (type-case Foo y
              [(Bar z) #t]
           [(Baz z) TODO]) 3 (TODO x y #t)) TODO)) |#
