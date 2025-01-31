#lang racket
(require "untyped.rkt"
         (only-in flit s-exp-content))

(define-syntax-rule (test a b)
  (unless (equal? a b)
    (error 'test "failed: ~.s" 'b)))

(test (s-exp-content x) '(a 2 "c" '(d)))
(test "ok" ((v-f i) "ok"))
(test add1 (v-f (v add1)))
(test #t (v? i))

(test "a" (f "a"))
(test 0 (f 0))

(test #t (ordinal? (ordinal "x")))

(test '(6 6) (apply-identity (lambda (x) 6) 5))



(test "dog?" (add-char "dog" #\?))

(test 65 (extract-first #"ABC"))
