#lang racket
(require rackunit)

(define (syn-test exp reg)
  (define ns (make-base-namespace))
  (check-regexp-match
   (if (string? reg) (regexp-quote reg) reg)
   (with-handlers ((exn:fail:syntax? exn-message))
     (parameterize ([current-namespace ns])
       (eval exp)
       "NO SYNTAX ERROR"))))

(syn-test
 '(module m flit
    
    (define-type TEnv
      (mt)
      (bind [x : symbol]
            [t : string]
            [rest : TEnv]))
    
    (define (lookup a-tenv id)
      (type-case TEnv a-tenv
        [mt (error 'lookup "ack!")]
        [(bind x t rest)
         (if (equal? x id)
             t
             (lookup rest))])))
 #rx"type-case: .*expected a parenthesized variant name.*mt.*")

;; Double-check value restrction:

          
;; ;; Check that polymorphism inference in nested scopes
;; ;; doesn't go wrong:
;; (syn-test
;;  '(module m flit
;;     
;;     (define member : ('a 'b -> 'c)
;;       (lambda (e l)
;;         #f))
;;     
;;     (local [(define (in? n)
;;               (member n n))]
;;       (if (string=? "in?" "in?") 
;;           in?
;;           (lambda (n) (void)))))
;;  #rx"typecheck failed: Void vs. Boolean")

(syn-test
 '(module m flit
    (quote #"x"))
 #rx"disallowed content")

(syn-test
 '(module m flit
    (quasiquote #"x"))
 #rx"disallowed content")

(syn-test
 '(module m flit
    (quasiquote unquote))
 #rx"bad syntax")

(syn-test
 '(module m flit
    (quasiquote (unquote 5)))
 #rx"Number vs. S-Exp")

(syn-test
 '(module m flit
    (quasiquote (1 (unquote-splicing 5) 3)))
 #rx"Number vs. .Listof S-Exp.")





(syn-test
 '(module m flit
    (define x "x")
    (module+ test (+ 1 x)))
 #rx"String vs. Number|Number vs. String")

(syn-test
 '(module m flit
    (case 1
      [(1) 5]
      [(a) 6]))
 #rx"number")

(syn-test
 '(module m flit
    (case 1
      [(a) 5]
      [(1) 6]))
 #rx"number")

(syn-test
 '(module m flit
    (case 1
      [(a) 5]
      [(b) 6]))
 #rx"Number vs. Symbol|Symbol vs. Number")

(syn-test
 '(module m flit
    (case 1
      [else 6]))
 #rx"Number vs. Symbol|Symbol vs. Number")

(syn-test
 '(module m flit
    (has-type 1 : Symbol))
 #rx"Number vs. Symbol|Symbol vs. Number")

(syn-test
 '(module m flit
    (define (->) 3))
 #rx"cannot redefine a keyword")

(syn-test
 '(module m flit
    (define-values (z ->) 3))
 #rx"cannot redefine a keyword")

(syn-test
 '(module m flit
    (define-values (z [-> : Number]) 3))
 #rx"cannot redefine a keyword")

(syn-test
 '(module m flit
    (2))
 #rx"call of a non-function")

(syn-test
 '(module m flit
    ((none)))
 #rx"call of a non-function")

(syn-test
 '(module m flit
    (define (f x) x)
   (f))
 #rx"wrong number of arguments")

(syn-test
 '(module m flit
   (define (f x) (x 1))
   (f 10))
 #rx"typecheck failed: [^\n]*vs.")

(syn-test
 '(module m flit
   (define (f x) (x 1))
   (f (lambda () 10)))
 #rx"typecheck failed: [^\n]*vs.")

(syn-test
 '(module m flit
    (type-case (Listof Number) '()))
 #rx"missing `empty` clause")

(syn-test
 '(module m flit
    (type-case (Listof Number) '()
      [empty 10]))
 #rx"missing `cons` clause")

(syn-test
 '(module m flit
    (type-case (Listof Number) '()
      [(cons x y) 10]))
 #rx"missing `empty` clause")

(syn-test
 '(module m flit
    (type-case (Listof Number) '()
      [(cons x y) 10]
      [(cons z q) 12]))
 #rx"variant already covered by a previous clause")

(syn-test
 '(module m flit
    (type-case (Listof Number) '()
      [(cons x 9) 10]
      [else 0]))
 #rx"expected an identifier")

(syn-test
 '(module m flit
    (type-case (Listof Number) '()
      [(cons x x) 10]
      [else 0]))
 #rx"duplicate binding variable for `cons`")

(syn-test
 '(module m flit
    (type-case (Listof String) '()
      [(cons x y) x]
      [else 0]))
 #rx"typecheck failed: String vs[.] Number")


(syn-test
 '(module m flit
    (type-case (Listof Number) '()
      [(cons x y) y]
      [else 0]))
 #rx"typecheck failed: .Listof Number. vs[.] Number")

(syn-test
 '(module m flit
    (define (f x)
      (let ([y x])
        y))
    (+ (f #t) 1))
 #rx"typecheck failed: Number vs[.] Boolean")


(syn-test
 '(module m flit
    (x :))
 #rx"declaration: expected a single type")

(syn-test
 '(module m flit
    (5 : Number))
 #rx"declaration: expected an identifier")

(syn-test
 '(module m flit
    (x : Number))
 #rx"declaration: identifier not defined here")

(syn-test
 '(module m flit
    (x : Number)
    (define x "Hello"))
 #rx"typecheck failed: Number vs[.] String")

(syn-test
 '(module m flit
    (define x "Hello")
    (x : Number))
 #rx"typecheck failed: Number vs[.] String")

(syn-test
 '(module m flit
    (define x "Hello")
    (x : Number))
 #rx"typecheck failed: Number vs[.] String")

;; (syn-test
;;  '(module m flit
;;     (lambda ([z : 'a])
;;       (local [(x : 'a)
;;               (y : 'a)
;;               (define x 10)
;;               (define y "apple")]
;;         (+ x (string-length y)))))
;;  #rx"generic")
