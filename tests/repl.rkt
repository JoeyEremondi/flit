#lang racket

(define ns (make-base-empty-namespace))
(parameterize ([current-namespace ns])
  (namespace-require 'flit))

(define (tl expect expr)
  (define s
    (with-output-to-string
      (lambda ()
        (parameterize ([current-namespace ns])
          ((current-print) (eval `(#%top-interaction . ,expr)))))))
  (unless (equal? s expect)
    (error 'test "failed: ~e vs. ~e" expect s)))

(define (te rx expr)
  (with-handlers ([exn:fail? (lambda (exn) 
                               (unless (regexp-match? rx (exn-message exn))
                                 (error 'test "failed: ~e vs. ~e" rx exn)))])
    (parameterize ([current-namespace ns])
      ((current-print) (eval `(#%top-interaction . ,expr))))
    (error 'test "failed (expected exn): ~.s" expr)))

(tl "- Number\n1\n" '1)
(tl "" '(define x 5))
(tl "- Number\n5\n" 'x)
(tl "" '(define x 6))
(tl "- Number\n6\n" 'x)

(te #rx"Number vs[.] String" '(define x "hello"))

(tl "" '(define b (box (list))))
(tl "- (Boxof (Listof '_a))\n'#&()\n" 'b)
(tl "- Void\n" '(set-box! b (list 'a)))
(tl "- (Boxof (Listof Symbol))\n'#&(a)\n" 'b)

(tl "" '(define-type (M 'a)
          [v (fd : 'a)]))
(te #rx"duplicate definition for identifier" '(define-type (M 'a)
                                                [M (v : 'a)]))

(tl "" '(define xb (box empty)))
(tl "" '(define yb (box empty)))
(te (regexp-quote "(Listof (Boxof (Listof '_a))) vs. (Boxof (Listof '_b))") '(cons xb yb))

;; Scope of type variables:
;(tl "" '(define f (lambda ([x : 'a] [y : 'b]) (has-type x : 'b))))
;(tl "- ('a 'a -> 'a)\n#<procedure:f>\n" 'f)
(tl "" '(define g (lambda ([x : 'a] [y : 'b]) x)))
(tl "- ('a 'b -> 'a)\n#<procedure:g>\n" 'g)
;; (tl "- (('_a '_a -> '_a) * ('_b '_c -> '_b))\n(values #<procedure:f> #<procedure:g>)\n"
;;     '(letrec ([f (lambda ([x : 'a] [y : 'b]) (has-type x : 'b))]
;;               [g (lambda ([x : 'a] [y : 'b]) x)])
;;        (values f g)))
(te (regexp-quote "generic") '(define x : 'a (cons (has-type 1 : 'a) empty)))
;; (tl "- (Number * (Number -> Number))\n(values 1 #<procedure:f>)\n"
;;     '(values
;;       (has-type 1 : Number)
;;       (letrec ([f (lambda ([x : 'a]) x)]) f)))
;; (tl "- (('_a -> '_a) * Number)\n(values #<procedure:f> 1)\n"
;;     '(values
;;       (letrec ([f (lambda ([x : 'a]) x)]) f)
;;       (has-type 1 : 'a)))
(te (regexp-quote "generic")
    '(lambda ([x : 'a])
       (local [(define one : 'a 1)
               (define two : 'a "two")]
         #f)))

(te #rx"type variable in an alias is not yet in scope" '(define-type-alias Foo 'a))
(tl "" '(define-type-alias (Foo 'a) 'a))

;; Should have no source inside plai-typed implementation:
(te "^typecheck failed" '(cond [#t 4] [#f "string"]))
(te "^typecheck failed" '(cond [#t 4] ["string" 5]))

(te "misplaced `else' clause"
    '(type-case (Optionof string) (some "x") [(some x) x] [else "else"] [(none) "none"]))

(te "must be an identifier"
    '(type-case (Optionof string) "frn" [(some ,w) 3]))


(tl "" (void))

;; Cannot redefine when polymorphic type
(tl "" '(define (idt x) x))
(tl "- ('a -> 'a)\n#<procedure:idt>\n" 'idt)
(te (regexp-quote "('a -> 'a) vs. ('_b -> '_b)")
    '(define (idt x) x))

(te "possible cycle"
    '(define (nstream x)
       (values 1 (nstream 10))))
