#lang racket/base
(require flit/private/force)



;; ----------------------------------------

(module sloth flit #:lazy
  (define-type Sloth
    [sloth (size : Number)])
  (define speedy (sloth (+ 1 2))))
(require 'sloth)

(unless (equal? "(sloth 3)" (format "~v" (!! speedy)))
  (error 'force "force didn't force"))
