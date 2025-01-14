#lang s-exp "main.rkt"

(define-type Foo
  [Bar (x : Number)]
  [Baz (y : Boolean)])

(define y (Bar 3))

(type-case Foo (Bar 3)
           [(Bar z) #t]
           [(Baz z) #t])

(define (f [x : Number] [y : Boolean] [z : (Number -> Number)]) : Boolean
  TODO)


empty
(empty)

(define (e? l)
  (type-case (Listof 'a) l
    [(empty) #t]
    [(cons h t) #f]))

  (define (concat [xs : (Listof 'elem)]
                  [ys : (Listof 'elem)])
          : (Listof 'elem)
    (type-case (Listof 'elem) xs
      [(empty)
         ys]
      [(cons h t)
         TODO]))

(define (testfun [x : String])
  (let* ([somevar 3])
    TODO))


#| (define (map (f : ('a -> 'b)) (lst : (Listof 'a))) : (Listof 'b)
     TODO)

   (define (f [x : 'b] [y : Foo]) : Number
     (+ (if (type-case Foo y
              [(Bar z) #t]
           [(Baz z) TODO]) 3 (TODO x y #t)) TODO)) |#
