#lang setup/infotab

(define collection "flit")

(define deps '(
               "base"
               "lazy"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     ["scribble-lib" #:version "1.16"]))

(define test-omit-paths '("scribblings/demo.rkt"))

(define version "1.1")

(define scribblings '(("scribblings/flit.scrbl" (multi-page) (language))))
