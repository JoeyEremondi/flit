#lang racket
(require "basic.rkt"
         (only-in flit s-exp-content))

(define-syntax-rule (test a b)
  (unless (equal? a b)
    (error 'test "failed: ~.s" 'b)))

(test (s-exp-content x) '(a 2 "c" '(d) #f))
(test "ok" ((v-f i) "ok"))
(test add1 (v-f (v add1)))
(test #t (v? i))

(test 'err (with-handlers ([exn:fail:contract? (lambda (exn) 'err)])
             (f "a")))
(test 0 (f 0))

(test 'err (with-handlers ([exn:fail:contract? (lambda (exn) 'err)])
             (ordinal "x")))

(test '(6 6) (apply-identity (lambda (x) 6) 5))





(test "dog?" (add-char "dog" #\?))

(test 65 (extract-first #"ABC"))
