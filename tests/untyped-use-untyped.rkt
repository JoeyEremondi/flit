#lang flit #:untyped
(require "untyped.rkt")



(test '(6 6) (apply-identity (lambda (x) 6) 5))


;;TODO something back here