#lang flit

(require (rename-in
          (typed-in racket/base
                    [expt : (Number Number -> Number)]
                    [+ : (Number Number -> Number)]
                    )
          [+ plus]))

(define x : S-Exp `(a 2 "c" '(d) #f))
(define (f [y : Number]) y)

(print-only-errors #t)

(test "" 1024 (expt 2 10))
(test "" 7 (plus 3 4))

(define apply-identity : (('a -> 'a) 'a -> (Listof 'a))
  (lambda (id x)
    (list (id x) (id x))))




(test "" 12 (+ (max (add1 7) 0) (min (sub1 5) 9)))

(test "" #t (zero? (- (ceiling (floor (remainder 5 3)))
                   (identity (modulo 5 3)))))
(test "" #t (odd? 7))
(test "" #f (even? 7))

(test "" 3 (length (list "a" "b" "c")))
(test "" "b" (list-ref (list "a" "b" "c") 1))
(test "" "b" (second (list "a" "b" "c")))
(test "" "c" (third (list "a" "b" "c")))
(test "" "d" (fourth (list "a" "b" "c" "d")))

(test "" #t (s-exp-list? x))
(test "" #f (s-exp-string? x))
(test "" #f (s-exp-symbol? x))
(test "" #f (s-exp-number? x))
(test "" #f (s-exp-boolean? x))
(test "" (symbol->s-exp 'a) (first (s-exp->list x)))
(test "" #t (s-exp-symbol? (first (s-exp->list x))))
(test "" #t (s-exp-number? (first (rest (s-exp->list x)))))
(test "" #t (s-exp-string? (first (rest (rest (s-exp->list x))))))
(test "" #t (s-exp-list? (first (rest (rest (rest (s-exp->list x)))))))
(test "" #t (s-exp-boolean? (first (rest (rest (rest (rest (s-exp->list x))))))))
(test "" #t (s-exp-string? (string->s-exp "a")))
(test "" #t (s-exp-number? (number->s-exp 2)))
(test "" #t (s-exp-boolean? (boolean->s-exp #f)))
(test "" #f (s-exp->boolean `#f))
(test "" #t (s-exp-list? (list->s-exp (list))))
(test "" #t (s-exp-list? (list->s-exp (list (number->s-exp 2)))))



(test "" 5 (call/cc (lambda (x) 5)))
(test "" 5 (let/cc x 5))

;; (test "" 7 (local [(define y (lambda (q) (+ q 3)))
;;                 (y : (Number -> Number))]
;;           (if (= 0 (call/cc (lambda (k)
;;                               (begin
;;                                 (set! y k)
;;                                 0))))
;;               (y 2)
;;               7)))
;; (test "" 7 (local [(define y (lambda (q) (+ q 3)))]
;;           (if (= 0 (let/cc k
;;                      (begin
;;                        (set! y k)
;;                        0)))
;;               (y 2)
;;               7)))

;; (test "" 15 (local [(x : 'a)
;;                  (y : 'a)
;;                  (define x 10)
;;                  (define y "apple")]
;;            (+ x (string-length y))))

;; (test "" 15 (local [(ident : ('a -> 'a))
;;                  (define (ident x)
;;                    x)]
;;            (+ (ident 10) (string-length (ident "hello")))))


(define il (list (lambda (x) x)))
(test "" 5 ((first il) 5))
(test "" "a" ((first il) "a"))

(define-type (T 'a) 
  (v [f : ('a -> 'a)])
  (ordinal [n : Number]))
(define i (v (lambda (x) x)))
(test "" 10 ((v-f i) 10))
(test "" "a" ((v-f i) "a"))
(test "" #t (v? i))
(test "" #f (v? (ordinal 8)))

(define-type-alias IntT (T Number))
(define-type-alias (XT 'x 'y) (T 'x))
(test "" 7 ((lambda ([i : IntT]) (type-case (T Number) i
                                [(v f) (f 6)]
                                [(ordinal n) n]))
         (v (lambda (x) (+ 1 x)))))
(test "" 7 ((lambda ([i : (XT Number String)]) (type-case (T Number) i
                                              [(v f) (f 6)]
                                              [(ordinal n) n]))
         (v (lambda (x) (+ 1 x)))))

(test "" 5 (type-case (T Number) i
          [(ordinal n) n]
          [else 5]))

(test "" #t (letrec ([even? (lambda (n)
                           (if (= 0 n)
                               #t
                               (odd? (- n 1))))]
                  [odd? (lambda (n)
                          (if (= 0 n)
                              #f
                              (even? (- n 1))))])
           (even? 10)))
;; (test "" (list 3 1 2) (let ([x 1]
;;                          [y 2]
;;                          [z 3])
;;                      (let ([x z]
;;                            [y x]
;;                            [z y])
;;                        (list x y z))))
(test "" 4 (let* ([x 1]
               [y 2]
               [x y]
               [y x])
          (+ y y)))

(test "" (list 2 4 6) (filter (lambda (x) (not (= x 5)))
                           (list 2 5 5 4 6 5)))
(test "" 10 (foldl (lambda (x n) (+ x n))
                0
                (list 1 2 3 4)))
(test "" "1234" (foldr (lambda (x n) (string-append (to-string x) n))
                    ""
                    (list 1 2 3 4)))

(test "" 2 (case 'apple
          [(donut) 1]
          [(apple banana) 2]
          [else 5]))

(test "" 5 (case 'apple
          [else 5]))

(test "" 2 (case 3
          [(0) 1]
          [(2 3) 2]
          [else 5]))

(test "" 7 (cond
         [(= 0 1) 6]
         [else 7]))

(test/exn "" (cond) "no matching")
(test/exn "" (cond [#f 10]) "no matching")
(test/exn "" (case 'apple) "no matching")
(test/exn "" (case 'apple [(banana) 12]) "no matching")

(define vd : Void (void))

(define-type SharedGraph$
  (node [s : String]
        [next : (Listof SharedGraph$)]))



(define n (if #f
              (+ (time 10) 1)
              0))

(test "" 5 (length (build-list 5 (lambda (i) (if (zero? i) "s" "f")))))



(test "" #t (member 1 (list 3 2 1)))
(test "" #f (member 6 (list 3 2 1)))

(test "" 1 (type-case (Optionof 'a) (none)
          [(none) 1]
          [(some v) 2]))
(test "" 5 (type-case (Optionof Number) (some 5)
          [(none) 1]
          [(some v) v]))

(test "" #t (some? (some 5)))
(test "" #t (none? (none)))
(test "" #f (none? (some 5)))
(test "" #f (some? (none)))

(define sid (some (lambda (x) x)))
(test "" 5 ((some-v sid) 5))
(test "" "5" ((some-v sid) "5"))

(test "" '(2 3) (type-case (Listof Number) '(1 2 3)
               [(cons a b) b]
               [empty empty]))
(test "" 1 (type-case (Listof Number) '(1 2 3)
          [(cons a b) a]
          [empty 5]))
(test "" 5 (type-case (Listof Number) '()
          [(cons a b) a]
          [empty 5]))

(define (my-length l)
  (type-case (Listof 'a) l
    [empty 0]
    [(cons a b) (+ 1 (my-length b))]))
(test "" 5 (my-length '(1 2 3 4 5)))
(test "" 4 (my-length '(a b c d)))




(test "" #t (s-exp-symbol? `a))
(test "" `3 (second (s-exp->list `(a ,(number->s-exp (+ 1 2)) c))))
(test "" `4 (third (s-exp->list `(a ,@(list `3 `4) c))))
(test "" (second (s-exp->list `(a `,@(list `3 `4) c)))
      `(quasiquote (unquote-splicing (list `3 `4))))

(define kons cons)


(module+ test
  (define (kons v) v)
  (define apple-string "apple")
  (define-type Fish (trout [n : Number]) (bass))
  )

(module+ test
  (test "" "apple" apple-string)
  (define (fry f)
    (type-case Fish f
      [(trout n) (+ n 1)]
      [(bass) 0])))

(include "for-basic.rktl")
(test "" "hello6" (string-append included-string
                              (to-string (+ included-num 1))))

(define-syntax twice
  (syntax-rules ()
    [(twice a)
     (+ a a)]
    [(twice a b)
     (+ (+ a a) (+ b b))]))
(test "" 10 (twice 5))
(test "" 16.2 (twice 6.0 2.1))

(define-syntax-rule (define-seven a b)
  (splice
   (define a 7)
   (define b 7)))
(define-seven seven-a seven-b)
(test "" 7 seven-a)
(test "" 7 seven-b)

(define-syntax (a-macro stx)
  #'(+ 1 2))
(test "" 3 (a-macro whatever you like))





(define (add-char s c) (string-append s (list->string (list c))))
(test "" #\a (string-ref "cat" 1))
(test "" "at" (substring "catch" 1 3))
(test "" 3 (string-length "cat"))
(test "" (list #\c #\a #\t) (string->list "cat"))
(test "" "cat" (list->string (list #\c #\a #\t)))
(test "" "cat!" (add-char "cat" #\!))

(test "" (list "cat") (let ([g (lambda (x) (list x))])
                     (let ([f g]) ; identifiers are values
                       (if #f
                           (if (zero? (first (f 10))) (list "a") (list "b"))
                           (f "cat")))))

(require (opaque-type-in racket/base
                         [Bstring bytes?])
         (typed-in racket/base 
                   [bytes : (Number Number -> Bstring)]
                   [bytes-ref : (Bstring Number -> Number)]))
(test "" 10 (bytes-ref (bytes 5 10) 1))
(define (extract-first s)
  (bytes-ref s 0))
(define (generate-bstring a b) : Bstring
  (bytes a b))

(define flonum? #t)
(require (rename-in (opaque-type-in racket/base
                                    [fl flonum?])
                    [fl Flonum]
                    [flonum? real-flonum?])
         (typed-in racket/base
                   [exact->inexact : (Number -> Flonum)])
         (typed-in racket/flonum
                   [fl+ : (Flonum Flonum -> Flonum)]))
(test "" (exact->inexact 10.0) (fl+ (exact->inexact 6) (exact->inexact 4)))

(define (poly g)
  ;; check that a non-polymorphic binding inside
  ;; doesn't break polymorphism of the enclosing
  ;; function
  (let ([x (g)])
    x))

(test "" 1 (poly (lambda () 1)))
(test "" "x" (poly (lambda () "x")))


;; Check expansion of a type abbreviation into
;; a type abbreviation:
(define-type-alias (MapFunc 'a 'b) ('a -> 'b))
(define-type-alias (Env 'v) (MapFunc Symbol 'v))
(define (lookup [var : Symbol]
                [env : (Env 'v)])
  (error 'NotImpl "NotImplemented"))

(test "" 5 (fst (pair 5 #t)))
(test "" #t (snd (pair 5 #t)))

(define (add-two l)
  (+ (first (has-type l : (Listof Number)))
     (has-type (second l) : Number)))
(test "" 3 (add-two (list 1 2)))


;; does define-values work correctly?
(define-values (aoeua aoeub) (values 3 "banana"))
(test "" 3 aoeua)
(test "" "banana" aoeub)
(define-values ([aoeuc : Number] [aoeud : String]) (values 4 "plum"))
(test "" 4 aoeuc)
(test "" "plum" aoeud)

(test "" (cond
        [(< 2 1) ....]
        [else 'ok])
      'ok)
(test "" (cond
        [(< 2 1) (.... 'oops)]
        [else 'ok])
      'ok)

(define tuple-test (values 1 2))

(splice
 (spliced-x : Number)
 (define spliced-x 1)
 (define spliced-y 2))


(define-type Spell
  (incantation))

(define-type-alias Incantation Spell)

(test "" (type-case Incantation (incantation)
        [(incantation) ""])
      "")

(define-type-alias (Magic 'x) (Listof 'x))
(define-type-alias Incantations (Magic Incantation))

(test "" (type-case Incantations (list (incantation))
        [empty 'no]
        [(cons inc ls) 'yes])
      'yes)

