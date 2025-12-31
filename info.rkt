#lang setup/infotab

(define collection "flit")

(define deps '(
               "base"
               "lazy"
               "todo-list"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     ["scribble-lib" #:version "1.16"]
                     "todo-list"))

(define test-omit-paths '("scribblings/demo.rkt"))

(define version "2026.01.02")

(define scribblings '(("scribblings/flit.scrbl" (multi-page) (language))))
