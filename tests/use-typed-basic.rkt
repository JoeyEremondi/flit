#lang flit
(require (rename-in "basic.rkt"
                    [seven-a Seven-A]))

(test 9 ((some-v sid) 9))
(test 'ok ((some-v sid) 'ok))

(test 7 (type-case (T Number) (ordinal 7)
          [(v f) (f 6)]
          [(ordinal n) n]))

(test 10 ((lambda ([x : IntT]) ((v-f x) 9)) (v add1)))

(define (sgf [sg : SharedGraph$]) sg)

(test (list) (unbox boxed-null))

(test 1 (length (list kons)))

(test 88 (twice 44))
(test 3 (a-macro whatever we like this time around))

(test 7 Seven-A)

(test 19 (parameterize ([prm 19])
           (get-prm)))

(define bs : Bstring (generate-bstring 65 78))
(test 65 (extract-first bs))

(test "hi"
      (type-case Linked-List (llnode "hi" (none))
        [(llnode s next) s]))

(test 14 (twice 7))
(test 3 (a-macro))

(define-seven qi more-qi)
(test 7 qi)
(test 7 more-qi)
