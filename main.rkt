#lang racket/base

(require (only-in "plai-datatype.rkt"
                  define-type
                  type-case
                  plai-stx-type?
                  validate-and-remove-type-symbol
                  )
         (only-in "plai-test.rkt"
                  test
                  test/exn
                  print-only-errors
                  (plai-error error))
         (prefix-in lazy: lazy)
         (prefix-in lazy: "private/lazy-datatype.rkt")
         racket/pretty
         racket/list
         racket/bool
         racket/local
         racket/shared
         racket/include
         syntax/wrap-modbeg
         (only-in racket/contract/base contract-out)
         racket/trace
         "private/fixup-quote.rkt"
         "private/s-exp.rkt"
         "private/tuple.rkt"
         "private/force.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/string
                     racket/syntax
                     racket/format
                     syntax/parse
                     syntax/transformer
                     racket/pretty
                     racket/port
                     racket/require-transform
                     "private/types.rkt"
                     racket/struct-info
                     "private/collapse.rkt"))

(provide :
         (rename-out [define: define]
                     [define-values: define-values]
                     [lambda: lambda]
                     [lambda: λ]
                     [begin: begin]
                     ;; [local: local]
                     [letrec: letrec] [let*: let] [let*: let*]
                     ;; [shared: shared]
                     [parameterize: parameterize]
                     [cond: cond]
                     [case: case]
                     [if: if]
                     ;; [when: when]
                     ;; [unless: unless]
                     [or: or]
                     [and: and]
                     [quote: quote]
                     [quasiquote: quasiquote]
                     ;; [set!: set!]
                     [time: time]
                     [trace: trace]
                     [require: require]
                     [module+: module+]
                     [include: include]
                     [splice: splice]
                     [define-syntax: define-syntax]
                     [define-syntax-rule: define-syntax-rule]
                     [#%app: #%app])
         TODO ;;JE
         #%datum #%top unquote unquote-splicing
         module submod
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction])
         else typed-in rename-in opaque-type-in
         has-type
         ....

         (for-syntax (all-from-out racket/base))

         (rename-out [define-type: define-type]
                     [type-case: type-case])
         define-type-alias

         (rename-out [test: test]
                     [test/exn: test/exn])
         test/noerror
         print-only-errors

         (rename-out [cons: cons]
                     [empty: empty]
                     [list: list]
                     [first: first]
                     [second: second]
                     [third: third]
                     [fourth: fourth]
                     [list-ref: list-ref]
                     [length: length]
                     [rest: rest]
                     [map: map]
                     [map2: map2]
                     [reverse: reverse]
                     [append: append]
                     [member: member]
                     [foldl: foldl]
                     [foldr: foldr]
                     [filter: filter])
         empty? cons?
         build-list

         + - = > < <= >= / * min max 
         add1 sub1 zero? odd? even?
         modulo remainder floor ceiling
         symbol=? string=? equal? eq? not
         error
         ;;try
         call/cc let/cc

         string->symbol symbol->string
         string-append to-string
         display

         char=? string-ref substring string-length string->list list->string

         ;; (rename-out [make-hash: make-hash]
         ;;             [hash: hash]
         ;;             [hash-ref: hash-ref])
         ;; hash-set! hash-remove! hash-keys
         ;; hash-set hash-remove

         s-exp-symbol? s-exp->symbol symbol->s-exp
         s-exp-number? s-exp->number number->s-exp
         s-exp-string? s-exp->string string->s-exp
         s-exp-boolean? s-exp->boolean boolean->s-exp
         s-exp-list? s-exp->list list->s-exp
         (rename-out [read: read])
         s-exp-match?

         ;; no type, so for use only in untyped:
         s-exp s-exp-content tuple tuple-content
         
         ;; (rename-out [box: box]
         ;;             [unbox: unbox]
         ;;             [set-box!: set-box!])

         ;; (rename-out [vector: vector]
         ;;             [vector-set!: vector-set!])
         ;; make-vector vector-ref vector-length

         (rename-out [values: values]
                     [pair: pair])
         fst snd
         (rename-out [fst pairFst]
                     [snd pairSnd])

         (rename-out [identity: identity])

         ;; make-parameter parameter-ref parameter-set!

         void
         
         (rename-out [number Number]
                     [boolean Boolean]
                     [symbol Symbol]
                     [string: String]
                     [char Char]
                     [s-expression S-Exp])
         -> 
         (rename-out [listof: Listof]
                     ;; [boxof: Boxof]
                     ;; [parameterof: Parameterof]
                     ;; [vectorof: Vectorof]
                     ;; [hashof: Hashof]
                     [void: Void])

         Optionof
         (rename-out [none: none]
                     [some: some]
                     [some-v: some-v]
                     [none?: none?]
                     [some?: some?]))


(pretty-print-depth #f)
(begin-for-syntax
  (pretty-print-depth #f)
  (define untyped? #f)
  (define lazy? #f)
  (define fuel 100)
  (define saw-submodules (make-hasheq))
  (define (log-stx stx [str "In macro:"])
    (println (list str stx)))
  (define types-for-locs (make-hash))
  (define mutable-ignored-vars '()))

;; Option is manually declared, basically "stdlib"
(define-type Optionof
  [none]
  [some (v (lambda (x) #t))])


;; Lazy options don't interoperate at all with eager ones
(lazy:define-type lazy-Optionof
                  [lazy-none]
                  [lazy-some (v (lambda (x) #t))])

(define not-there (gensym))


(define (hash: l)
  (apply hash
         (apply
          append
          (for/list ([v (in-list l)])
            (list (fst v) (snd v))))))

(define (make-hash: l)
  (make-hash (for/list ([v (in-list l)])
               (cons (fst v) (snd v)))))

(define (hash-ref: ht k)
  (define v (hash-ref ht k not-there))
  (if (eq? v not-there)
      (none)
      (some v)))

(define (s-exp-symbol? s) (symbol? (s-exp-content s)))
(define (s-exp->symbol s) (let ([v (s-exp-content s)])
                            (if (symbol? v) v (error 's-exp->symbol "not a symbol: ~e" s))))
(define (symbol->s-exp s) (s-exp s))
(define (s-exp-number? s) (number? (s-exp-content s)))
(define (s-exp->number s) (let ([v (s-exp-content s)])
                            (if (number? v) v (error 's-exp->number "not a number: ~e" s))))
(define (number->s-exp s) (s-exp s))
(define (s-exp-string? s) (string? (s-exp-content s)))
(define (s-exp->string s) (let ([v (s-exp-content s)])
                            (if (string? v) v (error 's-exp->string "not a string: ~e" s))))
(define (string->s-exp s) (s-exp s))
(define (s-exp-boolean? s) (boolean? (s-exp-content s)))
(define (s-exp->boolean s) (let ([v (s-exp-content s)])
                             (if (boolean? v) v (error 's-exp->boolean "not a boolean: ~e" s))))
(define (boolean->s-exp s) (s-exp s))
(define (s-exp-list? s) (list? (s-exp-content s)))
(define (s-exp->list s) (let ([v (s-exp-content s)])
                          (if (list? v) (map s-exp v) (error 's-exp->list "not a list: ~e" s))))
(define (list->s-exp s) (s-exp (map s-exp-content s)))

(define (identity x) x)
(lazy:define (lazy-identity x) x)

(define (read:)
  (define v (read))
  (unless (let loop ([v v])
            (or (symbol? v)
                (string? v) 
                (number? v)
                (boolean? v)
                (and (list? v)
                     (map loop v))))
    (error 'read "input is not an s-expression: ~e" v))
  (s-exp v))

(define (s-exp-match? pattern s)
  (local [;; main matching routine is called after checking
          ;; that the pattern is well-formed:
          (define (s-exp-match? pattern s)
            (cond
              [(s-exp-list? pattern)
               (and (s-exp-list? s)
                    (list-match? (s-exp->list pattern)
                                 (s-exp->list s)))]
              [(s-exp-number? pattern)
               (and (s-exp-number? s)
                    (= (s-exp->number s)
                       (s-exp->number pattern)))]
              [(s-exp-boolean? pattern)
               (and (s-exp-boolean? s)
                    (eq? (s-exp->boolean s)
                         (s-exp->boolean pattern)))]
              [(s-exp-string? pattern)
               (and (s-exp-string? s)
                    (equal? (s-exp->string s)
                            (s-exp->string pattern)))]
              [(eq? 'ANY (s-exp->symbol pattern))
               #t]
              [(eq? 'SYMBOL (s-exp->symbol pattern))
               (s-exp-symbol? s)]
              [(eq? 'NUMBER (s-exp->symbol pattern))
               (s-exp-number? s)]
              [(eq? 'STRING (s-exp->symbol pattern))
               (s-exp-string? s)]
              [else
               ;; Any other symbol is a literal:
               (and (s-exp-symbol? s)
                    (eq? (s-exp->symbol s)
                         (s-exp->symbol pattern)))]))
          ;; check a list of s-expr against a list of patterns,
          ;; handling '... among the patterns
          (define (list-match? patterns ses)
            (cond
              [(empty? patterns)
               (empty? ses)]
              [(and (cons? (rest patterns))
                    (s-exp-symbol? (first (rest patterns)))
                    (eq? '... (s-exp->symbol (first (rest patterns)))))
               ;; handle ...
               (cond
                 [(= (- (length patterns) 2)
                     (length ses))
                  ;; 0 matches may work
                  (list-match? (rest (rest patterns)) ses)]
                 [(empty? ses)
                  #f]
                 [else
                  ;; Need at least 1 match, then try again:
                  (and (s-exp-match? (first patterns)
                                     (first ses))
                       (list-match? patterns
                                    (rest ses)))])]
              [(empty? ses)
               #f]
              [else
               (and (s-exp-match? (first patterns)
                                  (first ses))
                    (list-match? (rest patterns)
                                 (rest ses)))]))

          ;; check that `pattern' is well-formed:
          (define (check-pattern pattern)
            (cond
              [(s-exp-list? pattern)
               (check-patterns (s-exp->list pattern) #f)]
              [(or (s-exp-boolean? pattern)
                   (s-exp-number? pattern)
                   (s-exp-string? pattern))
               (void)]
              [(not (s-exp-symbol? pattern))
               (error 's-exp-match? 
                      (string-append "bad pattern: "
                                     (to-string pattern)))]
              [(eq? '... (s-exp->symbol pattern))
               ;; if `check-patterns' didn't see it, it's misplaced
               (error 's-exp-shape? "misplaced `...' in pattern")]
              [else 
               ;; any other symbol is ok --- either special or literal
               (void)]))

          ;; check that a list of patterns is ok, possibly with
          ;; `...', but only one `...':
          (define (check-patterns patterns saw-dots?)
            (cond
              [(empty? patterns) (void)]
              [(and (cons? (rest patterns))
                    (s-exp-symbol? (first (rest patterns)))
                    (eq? '... (s-exp->symbol (first (rest patterns)))))
               (if saw-dots?
                   (error 's-exp-shape? "multiple `...' in pattern")
                   (check-patterns (rest (rest patterns)) #t))]
              [else
               (begin
                 (check-pattern (first patterns))
                 (check-patterns (rest patterns) saw-dots?))]))]
    (begin
      (check-pattern pattern)
      (s-exp-match? pattern s))))

(define (map2 f l1 l2) (map f l1 l2))
(define (lazy:map2 f l1 l2) (lazy:map f l1 l2))

(define-syntax (: stx)
  (raise-syntax-error
   #f
   "expected an expression instead of a colon"
   stx))

(define-for-syntax type
  (lambda (stx)
    (raise-syntax-error
     #f
     "expected an expression, found a type"
     stx)))

(define-syntax number type)
(define-syntax boolean type)
(define-syntax symbol type)
(define-syntax string: type)
(define-syntax char type)
(define-syntax s-expression type)
(define-syntax -> type)
(define-syntax listof: type)
(define-syntax boxof: type)
(define-syntax vectorof: type)
(define-syntax hashof: type)
(define-syntax parameterof: type)
(define-syntax void: type)

(define-syntax-rule (define-lazy-switch f: f lazy:f)
  (...
   (define-syntax (f: stx)
     (syntax-case stx ()
       [(_ arg ...)
        (if lazy?
            (syntax/loc stx (lazy:#%app lazy:f arg ...))
            (syntax/loc stx (f arg ...)))]
       [(_ . rest)
        (syntax/loc stx (f . rest))]
       [_ (datum->syntax #'here (if lazy? 'lazy:f 'f) stx)]))))
(define-lazy-switch cons: cons lazy:cons)
(define-lazy-switch list: list lazy:list)
(define-lazy-switch first: first lazy:first)
(define-lazy-switch second: second lazy:second)
(define-lazy-switch third: third lazy:third)
(define-lazy-switch fourth: fourth lazy:fourth)
(define-lazy-switch list-ref: list-ref lazy:list-ref)
(define-lazy-switch length: length lazy:length)
(define-lazy-switch rest: rest lazy:rest)
(define-lazy-switch map: map lazy:map)
(define-lazy-switch map2: map2 lazy:map2)
(define-lazy-switch reverse: reverse lazy:reverse)
(define-lazy-switch append: append lazy:append)
(define-lazy-switch filter: filter lazy:filter)
(define-lazy-switch foldl: foldl lazy:foldl)
(define-lazy-switch foldr: foldr lazy:foldr)

(define-lazy-switch box: box lazy:box)
(define-lazy-switch unbox: unbox lazy:unbox)
(define-lazy-switch set-box!: set-box! lazy:set-box!)

(define-lazy-switch vector: vector lazy:vector)
(define-lazy-switch vector-set!: vector-set! lazy:vector-set!)

(define-lazy-switch identity: identity lazy-identity)

(define-lazy-switch values: values/tuple lazy-values)
(define-lazy-switch pair: pair lazy-pair)

(define-lazy-switch none: none lazy-none)
(define-lazy-switch some: some lazy-some)
(define-lazy-switch some-v: some-v lazy-some-v)
(define-lazy-switch none?: none? lazy-none?)
(define-lazy-switch some?: some? lazy-some?)

(define (member? a l)
  (and (member a l) #t))
(define (lazy:member? a l)
  (and (lazy:member a l) #t))

(define-lazy-switch member: member? lazy:member?)

(define (to-string x) (format "~v" x))

(define (parameter-ref p)
  (p))
(define (parameter-set! p v)
  (p v))

(define-for-syntax (is-type-keyword? a)
  (and (identifier? a)
       (ormap (lambda (i)
                (free-identifier=? a i))
              (syntax->list
               #'(: number boolean symbol char s-expression
                    string: -> * listof: hashof:
                    boxof: vectorof: parameterof:
                    void: Optionof)))))

(define-for-syntax (is-keyword? a)
  (or (is-type-keyword? a)
      (and (identifier? a)
           (ormap (lambda (i)
                    (free-identifier=? a i))
                  (syntax->list
                   #'(else))))))

(define-for-syntax (check-defn-keyword id stx)
  (when (is-keyword? id)
    (raise-syntax-error 
     #f 
     "cannot redefine a keyword"
     stx
     id)))

(define-for-syntax ((parse-arg stx) arg)
  (if (identifier? arg)
      (if (is-keyword? arg)
          (raise-syntax-error 
           #f 
           (format "keyword used as an argument name~a" 
                   (if (is-type-keyword? arg)
                       " (maybe missing square brackets?)"
                       ""))
           stx
           arg)
          arg)
      (syntax-case arg (:)
        [(id : type)
         (identifier? #'id)
         #'id]
        [_ (raise-syntax-error
            #f
            "expected either <id> or `[<id> : <type>]' for function argument"
            stx
            arg)])))

(define-for-syntax (check-top k)
  (lambda (stx)
    (if (eq? 'module-begin (syntax-local-context))
        #`(module-begin #,stx)
        (k stx))))

(define-syntax try
  (check-top
   (syntax-rules (lambda:)
     [(try expr1 (lambda: () expr2))
      (with-handlers* ([exn:fail? (lambda (exn) expr2)])
        expr1)])))

(define-for-syntax (absolute-module-path s)
  (if (and (pair? s)
           (or (and (eq? (car s) 'submod)
                    (or (equal? (cadr s) ".")
                        (equal? (cadr s) "..")))
               (and (eq? (car s) 'quote))))
      (module-path-index-join
       s
       (eval #'(variable-reference->module-path-index
                (#%variable-reference))))
      s))

(define-syntax require:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ clause ...)
        untyped?
        (syntax/loc stx (require clause ...))]
       [(_ clause ...)
        (with-syntax ([(new-clause ...)
                       (map (lambda (clause)
                              (let loop ([clause clause])
                                (syntax-case clause (typed-in rename-in opaque-type-in :)
                                  [(typed-in lib
                                             [id : type]
                                             ...)
                                   (begin
                                     (let ([lib #'lib]
                                           [ids (syntax->list #'(id ...))])
                                       (unless (module-path? (syntax->datum lib))
                                         (raise-syntax-error #f "bad module path" stx lib))
                                       (for ([id (in-list ids)])
                                         (unless (identifier? id)
                                           (raise-syntax-error #f "expected an identifier" stx id))))
                                     (with-syntax ([lib (fixup-quote #'lib #'quote:)])
                                       (syntax/loc clause (only-in lib id ...))))]
                                  [(typed-in lib spec ...)
                                   (for ([spec (in-list (syntax->list #'(spec ...)))])
                                     (syntax-case spec (:)
                                       [(id : type) (void)]
                                       [(id something . _) 
                                        (and (identifier? #'id)
                                             (or (not (identifier? #'something))
                                                 (not (free-identifier=? #'something #':))))
                                        (raise-syntax-error
                                         #f
                                         (format "expected a colon after the identifier `~s'"
                                                 (syntax-e #'id))
                                         clause
                                         #'something)]
                                       [_ (raise-syntax-error
                                           #f
                                           "expected a specification of the form [<id> : <type>]"
                                           clause
                                           spec)]))]
                                  [(rename-in sub-clause [old-id new-id] ...)
                                   (let ([sub (loop #'sub-clause)])
                                     (define (check id)
                                       (unless (identifier? id) 
                                         (raise-syntax-error #f "expected an identifier" clause id)))
                                     (for ([old-id (in-list (syntax->list #'(old-id ...)))]
                                           [new-id (in-list (syntax->list #'(new-id ...)))])
                                       (check old-id)
                                       (check new-id))
                                     (with-syntax ([sub sub])
                                       (syntax/loc clause (rename-in sub [old-id new-id] ...))))]
                                  [(opaque-type-in lib [id predicate-id] ...)                                   
                                   (let ()
                                     (define (check id)
                                       (unless (identifier? id) 
                                         (raise-syntax-error #f "expected an identifier" clause id)))
                                     (for ([id (in-list (syntax->list #'(id ...)))]
                                           [predicate-id (in-list (syntax->list #'(predicate-id ...)))])
                                       (check id)
                                       (check predicate-id))
                                     (syntax/loc clause (only-in lib predicate-id ...
                                                                 ;; Also import predicate as `id':
                                                                 [predicate-id id] ...)))]
                                  [mp
                                   (module-path? (syntax->datum #'mp))
                                   (let ([s (syntax->datum #'mp)])
                                     (define xs (if (and (pair? s) 
                                                         (eq? (car s) 'quote)
                                                         ;; The following check is intended to
                                                         ;; allow access to modules at the top
                                                         ;; level that have only symbolic names,
                                                         ;; but `syntax-local-submodules' is broken
                                                         ;; in expand mode as of v5.3.3, so we skip it
                                                         ;; for now.
                                                         #;
                                                         (memq (cadr s) (syntax-local-submodules)))
                                                    ;; convert to `submod' form:
                                                    (list 'submod "." (cadr s))
                                                    ;; ok as-is:
                                                    s))
                                     (define (has-submodule? name)
                                       (module-declared? (absolute-module-path
                                                          (if (and (pair? xs) (eq? (car xs) 'submod))
                                                              `(,@xs ,name)
                                                              `(submod ,xs ,name)))
                                                         #t))
                                     (define typed? (has-submodule? 'flit))
                                     (unless typed?
                                       (when (module-declared? (absolute-module-path xs) #t)
                                         (raise-syntax-error #f
                                                             (if (has-submodule? 'untyped-flit)
                                                                 "not a typed `flit' module"
                                                                 "not a `flit' module")
                                                             stx
                                                             #'mp)))
                                     (fixup-quote
                                      (if typed?
                                          (let ([new-clause
                                                 (if (and (pair? s) (eq? (car s) 'submod))
                                                     (quasisyntax/loc clause (#,@#'mp flit))
                                                     (quasisyntax/loc clause (submod mp flit)))])
                                            (datum->syntax clause
                                                           (syntax-e new-clause)
                                                           clause
                                                           clause))
                                          clause)
                                      #'quote:))]
                                  [_
                                   (raise-syntax-error #f
                                                       "not a valid require specification"
                                                       stx
                                                       clause)])))
                            (syntax->list #'(clause ...)))])
          #'(require new-clause ...))]))))

(define-syntax typed-in
  (make-require-transformer
   (lambda (stx)
     (unless untyped?
       (raise-syntax-error #f "allowed only in `require'" stx))
     (syntax-case stx (:)
       [(_ lib [id : type] ...)
        (begin
          (let ([lib #'lib]
                [ids (syntax->list #'(id ...))])
            (unless (module-path? (syntax->datum lib))
              (raise-syntax-error #f "bad module path" stx lib))
            (for ([id (in-list ids)])
              (unless (identifier? id)
                (raise-syntax-error #f "expected an identifier" stx id))))
          (with-syntax ([lib (fixup-quote #'lib #'quote:)])
            (expand-import #'(only-in lib id ...))))]))))

(define-syntax opaque-type-in
  (make-require-transformer
   (lambda (stx)
     (unless untyped?
       (raise-syntax-error #f "allowed only in `require'" stx))
     (syntax-case stx ()
       [(_ lib [id pred-id] ...)
        (begin
          (let ([lib (fixup-quote #'lib #'quote:)]
                [ids (syntax->list #'(id ... pred-id ...))])
            (unless (module-path? (syntax->datum lib))
              (raise-syntax-error #f "bad module path" stx lib))
            (for ([id (in-list ids)])
              (unless (identifier? id)
                (raise-syntax-error #f "expected an identifier" stx id))))
          (expand-import #'(only-in lib pred-id ... [pred-id id] ...)))]))))

(define-syntax test:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ e ...) (if lazy?
                      (syntax/loc stx (test (!! e) ...))
                      (syntax/loc stx (test e ...)))]))))

(define-syntax test/exn:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ e ...) (if lazy?
                      (syntax/loc stx (test/exn (!! e) ...))
                      (syntax/loc stx (test/exn e ...)))]))))

;; Shorthand for making sure a term runs without error
;; without caring what it returns
(define-syntax-rule (test/noerror t)
  (test (let ([_ t]) #t) #t))

(define-syntax (module+: stx)
  (define (module-begin-there s)
    (datum->syntax stx (syntax-e s) s s))
  (syntax-case stx ()
    [(_ name e ...)
     (with-syntax ([(decl ...)
                    (cond
                      [(hash-ref saw-submodules (syntax-e #'name) #f)
                       '()]
                      [else
                       (hash-set! saw-submodules (syntax-e #'name) #t)
                       (cond
                         [lazy?
                          #'(#:lazy (is-submodule))]
                         [else
                          #'((is-submodule))])])])
       (module-begin-there
        (cond
          [untyped?
           (syntax/loc stx
             (module+ name decl ... e ...))]
          [else
           (syntax/loc stx
             (module+ name
               decl ...
               ;; to register implicitly imported types:
               (begin (require (only-in (submod ".." flit))))
               e
               ...))])))]))

(define-syntax include: 
  (check-top
   (lambda (stx)
     (unless (memq (syntax-local-context) '(module top-level))
       (raise-syntax-error #f "allowed only as a top-level form" stx))
     (syntax-case stx ()
       [(_ spec) (with-syntax ([orig-stx stx])
                   (syntax/loc stx 
                     (include-at/relative-to orig-stx orig-stx spec)))]))))

(begin-for-syntax 
  (struct typed-macro (proc)
    #:property prop:procedure 0)
  (define macro-inspector (current-code-inspector))

  (define module-level-expansions null)

  (define (stash-and-add-begin proc)
    (lambda (stx)
      (define result (proc stx))
      (when (eq? (syntax-local-context) 'module)
        ;; We need to save the expansion so that we map macro-introduced
        ;; defined identifiers in the environment as the actually bound
        ;; identifier, and so that (in the macro of a macro-defining macro)
        ;; we get expansions that refer to those identifiers:
        (set! module-level-expansions
              (cons (cons stx (syntax-local-introduce (disarm result)))
                    module-level-expansions)))
      (if (syntax? result)
          ;; Insert a `begin' wrapper so we can `local-expand' just once:
          #`(begin #,result)
          ;; Otherwise, let expander report the error:
          result))))

(define-syntax define-syntax:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ id rhs)
        untyped?
        (syntax/loc stx (define-syntax id rhs))]
       [(_ id rhs)
        (identifier? #'id)
        (syntax-case* #'rhs (syntax-rules lambda) free-transformer-identifier=?
          [(syntax-rules . _)
           (syntax/loc stx
             (define-syntax id (typed-macro (stash-and-add-begin rhs))))]
          [(lambda (arg) . rest)
           (syntax/loc stx
             (define-syntax id (typed-macro (stash-and-add-begin rhs))))]
          [_
           (raise-syntax-error #f "expected a `syntax-rules' or single-argument `lambda' form after identifier" stx)])]
       [(_ (id arg-id) . rest)
        (and (identifier? #'id)
             (identifier? #'arg-id))
        (if (and (pair? (syntax-e #'rest))
                 (syntax->list #'rest))
            (syntax/loc stx
              (define-syntax id (typed-macro (stash-and-add-begin (lambda (arg-id) . rest)))))
            (raise-syntax-error #f "ill-formed macro body" stx))]
       [(_ id . _)
        (raise-syntax-error #f "expected an identifier or `(<identifier> <identifier>)' header" stx #'id)]))))

(define-syntax define-syntax-rule:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ pat tmpl)
        untyped?
        (syntax/loc stx (define-syntax-rule pat tmpl))]
       [(_ (id . rest) tmpl)
        (identifier? #'id)
        #`(define-syntax: id (syntax-rules () [(id . rest) tmpl]))]))))

(define-for-syntax (disarm stx)
  (let loop ([e stx])
    (cond
      [(syntax? e) 
       (define stx (syntax-disarm e macro-inspector))
       (datum->syntax stx
                      (loop (syntax-e stx))
                      stx
                      stx)]
      [(pair? e) (cons (loop (car e)) (loop (cdr e)))]
      [(vector? e) (list->vector (map loop (vector->list e)))]
      [else e])))

(define-for-syntax (local-expand-typed expr)
  (define stx (local-expand expr 'expression #f))
  (syntax-case stx (begin)
    [(begin e) (disarm #'e)]
    [_ (error 'local-expand-typed "something went wrong: ~e => ~e" expr stx)]))

(define-for-syntax (local-expand-typed-toplevel expr)
  (cond
    [module-level-expansions
     (when (or (null? module-level-expansions)
               (not (equal? (syntax->datum (caar module-level-expansions))
                            (syntax->datum expr))))
       (error 'flit "unexpected expansion: ~.s" (syntax->datum expr)))
     (begin0
       (cdar module-level-expansions)
       (set! module-level-expansions (cdr module-level-expansions)))]
    [else (local-expand-typed expr)]))

(define-for-syntax (expand-includes l)
  (let loop ([l l])
    (cond
      [(null? l) null]
      [else
       (syntax-case (car l) (include: splice:)
         [(include: spec)
          (append
           (cdr (syntax->list (local-expand (car l) (syntax-local-context) #f)))
           (loop (cdr l)))]
         [(splice: e ...)
          (loop (append (syntax->list #'(e ...)) (cdr l)))]
         [(id . _)
          (and (identifier? #'id)
               (typed-macro? (syntax-local-value #'id (lambda () #f))))
          (loop (cons (local-expand-typed-toplevel (car l))
                      (cdr l)))]
         [_ (cons (car l) (loop (cdr l)))])])))

(define-syntax splice: 
  (check-top
   (lambda (stx)
     (unless (memq (syntax-local-context) '(module top-level))
       (raise-syntax-error #f "allowed only as a top-level form" stx))
     (syntax-case stx ()
       [(_ e ...) #'(begin (drop-type-decl e) ...)]))))

(define-syntax define:
  (check-top
   (lambda (stx)
     (with-syntax ([define (if lazy? #'lazy:define #'define)])
       (syntax-case stx (:)
         [(_ id expr)
          (identifier? #'id)
          (begin
            (check-defn-keyword #'id stx)
            (syntax/loc stx
              (define id expr)))]
         [(_ id : type expr)
          (identifier? #'id)
          (begin
            (check-defn-keyword #'id stx)
            (syntax/loc stx
              (define id expr)))]
         [(_ (id arg ...) : type expr)
          (identifier? #'id)
          (begin
            (check-defn-keyword #'id stx)
            (with-syntax ([(arg ...)
                           (map (parse-arg stx) (syntax->list #'(arg ...)))])
              (syntax/loc stx
                (define (id arg ...) (#%expression expr)))))]
         [(_ (id arg ...) expr)
          (identifier? #'id)
          (begin
            (check-defn-keyword #'id stx)
            (with-syntax ([(arg ...)
                           (map (parse-arg stx) (syntax->list #'(arg ...)))])
              (syntax/loc stx
                (define (id arg ...) (#%expression expr)))))])))))

(define values/tuple (lambda args (tuple (apply vector-immutable args))))
(define (pair a b) (tuple (vector-immutable a b)))
(define (fst v) (vector-ref (tuple-content v) 0))
(define (snd v) (vector-ref (tuple-content v) 1))

(define lazy-values (lazy:lambda args (tuple (apply vector-immutable args))))
(lazy:define (lazy-pair a b) (tuple (lazy:vector-immutable a b)))

(define-syntax define-values:
  (check-top
   (lambda (stx)
     (syntax-case stx (:)
       [(_ (id ...) expr)
        (with-syntax ([(id ...)
                       (map (lambda (id)
                              (if (identifier? id)
                                  (begin (check-defn-keyword id stx)
                                         id)
                                  (syntax-case id (:)
                                    [(id : type)
                                     (begin (check-defn-keyword #'id stx) #'id)]
                                    [else
                                     (raise-syntax-error
                                      #f
                                      "expected <id> or `[<id> : <type>]'"
                                      stx
                                      id)])))
                            (syntax->list #'(id ...)))])
          (syntax/loc stx
            (define-values (id ...) (vector->values (tuple-content expr)))))]))))

(define-syntax lambda:
  (check-top
   (lambda (stx)
     (with-syntax ([lambda (if lazy? #'lazy:lambda #'lambda)])
       (syntax-case stx (:)
         [(_ (arg ...) : type expr)
          (with-syntax ([(arg ...)
                         (map (parse-arg stx) (syntax->list #'(arg ...)))])
            (syntax/loc stx
              (lambda (arg ...) (#%expression expr))))]
         [(_ (arg ...) expr)
          (with-syntax ([(arg ...)
                         (map (parse-arg stx) (syntax->list #'(arg ...)))])
            (syntax/loc stx
              (lambda (arg ...) (#%expression expr))))])))))

;;JE
(begin-for-syntax
  (struct command (name module-path function arguments) #:prefab))


;; For more control over the placement of TODOs, use the located struct.
;; For the ability to write separate summaries and detailed TODOs, use the todo-item struct.
(begin-for-syntax
  (struct located (loc value) #:prefab)
  (struct todo-item (full summary) #:prefab))

(define-for-syntax (pretty-type ty)
  (substring (format "~v" ((type->datum (make-hash)) ty)) 1))

(define-for-syntax (pretty-env env)
  (string-join
   (filter-map
    (lambda (env-entry)
      (and
       (debugln "CHECKING : ~s, compare to ~s" (syntax->datum (car env-entry)) mutable-ignored-vars)
       (not (member (car env-entry) mutable-ignored-vars free-identifier=?))
       (format "~a : ~a" (syntax->datum (car env-entry)) (pretty-type (cdr env-entry)))))
    env)
   "\n"))

(define-for-syntax (make-TODO stx pos)
  ;; (debugln "Checking for pos ~s in hash ~s\nin? ~s" pos types-for-locs (in-hash types-for-locs pos))
  ;; (unless
  ;;   (raise-syntax-error #f (format "No type info for expression at at position ~s\n Possible cause: expression where type was expected?" pos ) stx))
  (define item
    (if (hash-has-key? types-for-locs pos)
        (let* (
               (tyEnv (hash-ref types-for-locs pos ))
               (ty (car tyEnv))
               (env (cdr tyEnv))
               (ty-str (pretty-type ty))
               (context-str (string-append (pretty-env env) "\n_____________________________\nTODO : " ty-str)))
          (todo-item context-str ty-str))
        ;; Don't try to make a TODO item if we can't find the type
        ;; probably means there was a type error somewhere
        (todo-item "Error while typechecking" "Error while typechecking")))
  ;; Expand a TODO to a runtime error
  (define runtime
    (with-syntax ([lineNum (syntax-line stx)])
      (syntax/loc stx
        (error (format "TODO encountered at run-time, line ~s" lineNum)))))
  ;; Attach a notice that it is a TODO to be filled out
  (syntax-property
   (syntax-property
    runtime
    'todo item)
   'editing-command (command "Replace with error" "test-command.rkt" 'replace-with-error '())))

;; The simplest way to attach TODOs is to attach a string to the 'todo syntax
;; property.
(define-syntax (TODO stx)
  (define stx-pos (syntax-position stx))
  (syntax-parse stx
    [(td)
     (make-TODO stx stx-pos)]
    [_:id
     ;(log-stx stx "ID case")
     (make-TODO stx stx-pos)]
    [(td . args)
     #`(#,(make-TODO #'td (syntax-position #'td)) . args)]
    ;[_ (error "Error rest")]
    ))

(define-syntax TODO-function
  (make-variable-like-transformer #'(TODO-macro)))



(require racket/stxparam racket/splicing)

;; This example uses a syntax parameter to propagate the surrounding expression
;; context, attaching the todo-item to the context if one exists.
(define-syntax-parameter definition-context #f)

(define-syntax (define/todos stx)
  (syntax-parse stx
    [(_ x e)
     (with-syntax ([ctx stx])
       (syntax/loc stx
         (splicing-syntax-parameterize ([definition-context #'ctx])
           (define x e))))]))

(define-syntax (inner-TODO stx)
  (define ctx (or (syntax-parameter-value #'definition-context) stx))
  (syntax-parse stx
    [(_ msg:str)
     (define item
       (located ctx
                (todo-item (syntax->datum #'msg)
                           (syntax-parse ctx
                             #:literals (define/todos)
                             [(define/todos x e) (syntax->datum #'x)]
                             [_ (syntax->datum #'msg)]))))
     (syntax-property (syntax/loc stx (error 'inner-todo msg)) 'todo item)]))




;;; Example of an editing command without a goal.
(define-syntax (with-command stx)
  (syntax-case stx ()
    [(_ e)
     (syntax-property #'e
                      'editing-command
                      (command "Double" "test-command.rkt" 'double '()))]))


(define-syntax (TODO-bindings stx)
  (syntax-case stx ()
    [(_ e ...)
     ;; syntax-debug-info returns information about macro expansion contexts.
     ;; It will often let you get local variables in #lang racket! See the docs
     ;; for more information about syntax-debug-info.
     (let* ((info (syntax-debug-info stx (syntax-local-phase-level) #t))
            (bindings (hash-ref info 'bindings '())))
       (let* ((details (with-output-to-string
                         (lambda ()
                           (printf "Local vars:\n")
                           (for ([b bindings])
                             (let ((name (hash-ref b 'name #f))
                                   (local? (hash-ref b 'local #f)))
                               (when (and name local?)
                                 (printf " ~a\n" name)))))))
              (foo (println details)))
         (syntax-property #'(error 'TODO/bindings)
                          'todo (todo-item details "TODO with bindings"))))]))



(define-syntax (#%app: stx)
  (syntax-case stx ()
    [(_ e ...)
     (if lazy?
         (syntax/loc stx (lazy:#%app e ...))
         (syntax/loc stx (#%app e ...)))]))

(begin-for-syntax
  ;; Used to declare a variant name so that `shared' can create instances
  (struct constructor-syntax (id selectors mutators)
    #:property prop:set!-transformer
    (lambda (c stx)
      (with-syntax ([id (syntax-property (constructor-syntax-id c)
                                         'constructor-for
                                         (syntax-case stx (set!)
                                           [(set! id . _) #'id]
                                           [(id . _) #'id]
                                           [_ stx]))])
        (syntax-case stx (set!)
          [(set! _ rhs) (syntax/loc stx (set! id rhs))]
          [(_ arg ...) (syntax/loc stx (#%app: id arg ...))]
          [_ #'id])))
    #:property prop:struct-info
    (lambda (c)
      (list #f 
            (constructor-syntax-id c)
            #f
            (reverse (constructor-syntax-selectors c))
            (reverse (constructor-syntax-mutators c))
            #f)))
  (struct reprovided-constructor-syntax (id transformer)
    #:property prop:procedure (struct-field-index transformer)))

(define-for-syntax expand-define-type
  (lambda (stx)
    (syntax-case stx (:)
      [(_ thing . rest)
       (not (or (identifier? #'thing)
                (syntax-case #'thing (quote:)
                  [(id (quote: arg) ...)
                   (and (identifier? #'id)
                        (andmap identifier? (syntax->list #'(arg ...))))]
                  [_ #f])))
       (raise-syntax-error
        #f
        "expected an <id> for a type name or (<id> '<id> ...) for polymorphic type"
        stx
        #'thing)]
      [(_ id [variant (field : type) ...] ...)
       (with-syntax ([id (if (identifier? #'id)
                             #'id
                             (syntax-case #'id (quote:)
                               [(id (quote: arg) ...)
                                #'id]))]
                     [($variant ...) (map (lambda (variant)
                                            (datum->syntax variant
                                                           (string->uninterned-symbol
                                                            (symbol->string (syntax-e variant)))
                                                           variant
                                                           variant))
                                          (syntax->list #'(variant ...)))]
                     [((variant? (variant-field set-variant-field!) ...) ...)
                      (map (lambda (variant fields)
                             (cons 
                              (datum->syntax variant
                                             (string->symbol
                                              (format "~a?" (syntax-e variant)))
                                             variant
                                             variant)
                              (map (lambda (field)
                                     (define (mk fmt)
                                       (datum->syntax variant
                                                      (string->symbol
                                                       (format fmt
                                                               (syntax-e variant)
                                                               (syntax-e field)))
                                                      variant
                                                      variant))
                                     (list (mk "~a-~a") (mk "set-~a-~a!")))
                                   (syntax->list fields))))
                           (syntax->list #'(variant ...))
                           (syntax->list #'((field ...) ...)))])
         (let ([dup (check-duplicate-identifier
                     (syntax->list #'(id
                                      variant ... 
                                      variant? ...
                                      variant-field ... ... 
                                      set-variant-field! ... ...)))])
           (when dup
             (raise-syntax-error #f 
                                 "duplicate definition for identifier"
                                 stx
                                 dup)))
         (let ([s (with-syntax ([define-type (if lazy? #'lazy:define-type #'define-type)])
                    #'(define-type id
                        [$variant (field (lambda (x) #t)) ...] ...))])
           #`(begin
               #,(datum->syntax stx (syntax-e s) stx stx)
               (define-syntax variant (constructor-syntax
                                       (quote-syntax $variant)
                                       (list (quote-syntax variant-field)
                                             ...)
                                       (list (quote-syntax set-variant-field!)
                                             ...)))
               ...)))]
      [(_ id thing ...)
       (for-each (lambda (thing)
                   (syntax-case thing ()
                     [[variant thing ...]
                      (for-each (lambda (thing)
                                  (syntax-case thing (:)
                                    [(field : type) 'ok]
                                    [_ (raise-syntax-error
                                        #f
                                        "expected `(<id> : <type>)'"
                                        stx
                                        thing)]))
                                (syntax->list #'(thing ...)))]
                     [_ (raise-syntax-error
                         #f
                         "expected `[<id> (<id> : <type>) ...]'"
                         stx
                         thing)]))
                 (syntax->list #'(thing ...)))])))

(define-syntax define-type:
  (check-top expand-define-type))

(begin-for-syntax
  (struct type-alias (args rhs)))

(define-syntax define-type-alias
  (check-top
   (lambda (stx)
     (syntax-case stx (quote:)
       [(_ (id (quote: arg) ...) t)
        (begin
          (map (lambda (id)
                 (unless (identifier? id)
                   (raise-syntax-error #f "expected an identifier (e.g. a type variable)" stx id)))
               (syntax->list #'(id arg ...)))
          (check-defn-keyword #'id stx)
          #`(define-syntax id (type-alias (list (quote-syntax arg) ...) (quote-syntax t))))]
       [(_ id t)
        (begin
          (let ([id #'id])
            (unless (identifier? id)
              (raise-syntax-error #f "expected `<id>' or `(<id> '<id> ...)'" stx id))
            (check-defn-keyword #'id stx))
          #`(define-syntax id (type-alias null (quote-syntax t))))]))))

(define-syntax begin:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ e0 e ...)
        (with-syntax ([body (syntax/loc stx
                              (begin e0 e ...))])
          #'(#%expression body))]))))

(define-syntax local:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ (defn ...) e)
        (with-syntax ([(defn ...)
                       (for/list ([defn (in-list (syntax->list #'(defn ...)))]
                                  #:unless (syntax-case defn (:)
                                             [(_ : . _) #t]
                                             [_ #f]))
                         (define (no-local-type) (raise-syntax-error
                                                  #f
                                                  "local type definitions are not supported"
                                                  defn))
                         (syntax-case defn (define: define-values:)
                           [(define: . _) defn]
                           [(define-values: . _) defn]
                           [(define-type: . _) (no-local-type)]
                           [(define-type-alias: . _) (no-local-type)]
                           [else (raise-syntax-error
                                  #f
                                  "expected a function definition, constant definition, tuple definition, or type declaration"
                                  defn)]))])
          (syntax/loc stx
            (local (defn ...) (#%expression e))))]))))

(define-syntax parameterize:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ ([param-expr rhs-expr] ...) e)
        (syntax/loc stx
          (parameterize ([param-expr rhs-expr] ...) e))]))))

(define-for-syntax (make-let kind)
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ ([id rhs] ...) body)
        (let ([ids (syntax->list #'(id ...))])
          (for ([id (in-list ids)])
            (unless (identifier? id) (raise-syntax-error #f
                                                         "expected an identifier"
                                                         stx
                                                         id)))
          (case kind
            [(letrec) (syntax/loc stx (local: [(define: id rhs) ...] body))]
            [(let) (let* ([temps (generate-temporaries ids)]
                          ;; Record the variables to ignore
                          [_ (set! mutable-ignored-vars (append temps mutable-ignored-vars))])
                     (with-syntax ([(tmp ...) temps])
                       (begin
                         ;; Add the temps to variables not to print in TODOs
                         (syntax/loc stx
                           (local: [(define: tmp rhs) ...]
                                   (local: [(define: id tmp) ...]
                                           body))))))]
            [(let*) (let loop ([ids ids]
                               [rhss (syntax->list #'(rhs ...))])
                      (cond
                        [(empty? ids) #'body]
                        [else (let* ([temps (generate-temporaries (list (car ids)))]
                                     [_ (set! mutable-ignored-vars (append temps mutable-ignored-vars))])
                                (with-syntax ([body (loop (cdr ids) (cdr rhss))]
                                              [id (car ids)]
                                              [rhs (car rhss)]
                                              [tmp (car temps)])
                                  (syntax/loc stx
                                    (local: [(define: tmp rhs)]
                                            (local: [(define: id tmp)]
                                                    body)))))]))]))]))))

(define-syntax letrec: (make-let 'letrec))
(define-syntax let: (make-let 'let))
(define-syntax let*: (make-let 'let*))

(define-syntax shared:
  (check-top
   (lambda (stx)
     (if (eq? (syntax-local-context) 'expression)
         (syntax-case stx ()
           [(_ ([id rhs] ...) body)
            (let ([ids (syntax->list #'(id ...))])
              (for ([id (in-list ids)])
                (check-defn-keyword id stx))
              (syntax/loc stx
                (shared ([id rhs] ...) body)))])
         #`(#%expression #,stx)))))

(define-for-syntax (convert-clauses stx)
  ;; Preserve srcloc of each clause:
  (map (lambda (clause)
         (syntax-case clause (else)
           [[(variant id ...) ans]
            (for-each (lambda (id)
                        (when (is-keyword? id)
                          (raise-syntax-error 
                           #f 
                           "cannot bind a keyword"
                           stx
                           id)))
                      (syntax->list #'(id ...)))
            (with-syntax ([$variant (syntax-case #'variant (none: some: else)
                                      [none: (if lazy? #'lazy-none #'none)]
                                      [some: (if lazy? #'lazy-some #'some)]
                                      [else (raise-syntax-error #f "not allowed as an expression in parentheses" #'variant)]
                                      [_
                                       (let ([c (syntax-local-value #'variant (lambda () #f))])
                                         (cond
                                           [(constructor-syntax? c)
                                            (let ([id (constructor-syntax-id c)])
                                              (datum->syntax id (syntax-e id) #'variant))]
                                           [(reprovided-constructor-syntax? c)
                                            (let ([id (reprovided-constructor-syntax-id c)])
                                              (datum->syntax id (syntax-e id) #'variant))]
                                           [else #'variant]))])])
              (syntax/loc clause
                [$variant (id ...) (#%expression ans)]))]
           [[else ans]
            (syntax/loc clause
              [else (#%expression ans)])]))
       (syntax-case stx ()
         [(_ type expr clause ...) 
          (syntax->list #'(clause ...))])))

(define-for-syntax (signal-typecase-syntax-error stx)
  (syntax-case stx ()
    [(_ type expr clause ...)
     (let* ([clauses (syntax->list #'(clause ...))]
            [len (length clauses)])
       (for ([clause (in-list clauses)]
             [pos (in-naturals)])
         (syntax-case clause (else)
           [[(variant id ...) ans]
            (identifier? #'variant)
            'ok]
           [[else ans]
            (if (= pos (sub1 len))
                'ok
                (raise-syntax-error
                 #f
                 "misplaced `else' clause"
                 stx
                 clause))]
           [[something . rest]
            (not (pair? (syntax-e #'something)))
            (raise-syntax-error
             #f
             "expected a parenthesized variant name and field identifiers"
             stx
             #'something)]
           [[(var . _) . rest]
            (not (identifier? #'var))
            (raise-syntax-error
             #f
             "expected an identifier from a define-type"
             stx
             #'var)]
           [[(var . ids) . rest]
            (syntax-case #'ids ()
              [(x ...) (andmap identifier? (syntax->list #'(x ...))) #f]
              [else #t])
            (raise-syntax-error
             #f
             (format "second piece of the ~a clause must be a sequence of identifiers"
                     (syntax-e #'var))
             stx
             clause)]
           [[(var . ids) ans1 ans2 . ans]
            (raise-syntax-error
             #f
             "clause does not contain a single result expression"
             stx
             clause)]
           [[(var id ...) ans ...]
            (andmap identifier? (syntax->list #'(id ...)))
            (raise-syntax-error
             #f
             "clause does not contain a single result expression"
             stx
             clause)]
           [else (raise-syntax-error
                  #f
                  "ill-formed clause"
                  stx
                  clause)])))]
    [else
     (raise-syntax-error #f "ill-formed type-case" stx)]))

;; ;; Type-case that checks if it's given a type or not,
;; ;; and infers the type if it's not given one
(define-syntax bad-type-case:
  (check-top
   (lambda (stx)
     ;;(displayln (format "type-case syntax ~s" stx))
     (define result (syntax-case stx (else listof:)
                      ;; Check if it's a bad type
                      [(_ thing . rest)
                       (not (or (identifier? #'thing)
                                (syntax-case #'thing ()
                                  [(id arg ...)
                                   (identifier? #'id)]
                                  [_ #f])))
                       (raise-syntax-error
                        #f
                        "expected an <id> for a type name or `(<id> <type> ...)' for polymorphic type"
                        stx
                        #'thing)]
                      ;; Good syntax, where type is given
                      [(_ ty expr clause ...)
                       ;; Check that the thing we gave is actually a type
                       ;; otherwise we'll try to infer it
                       (with-handlers ([exn? (lambda (exn) #f)])
                         (or (and (list? #'ty) (eq? (car #'ty 'Listof))) (plai-stx-type? (syntax-local-value/record #'ty (lambda (x) #t)))))
                       #`(type-case: ty expr clause ...)]
                      ; [(_ expr clause ...)
        
                      ;; Check that the thing we gave is actually a type
                      ;; otherwise we'll try to infer it
                      ;(begin
                      ;;(displayln (format "Expanded: ~s" (do-original-typecheck #`expr)))
                      ;; (displayln types-for-locs)
                      ;;(displayln (format "Scrutinee type ~s" (hash-ref types-for-locs (syntax-position #'expr))))
                      ;  #t)
                      ;  #`(type-case: ty expr clause ...)]
                      ))
     ;;(displayln (format "Result: ~s" result)
     ;;)
     result)))

(define-syntax type-case:
  (check-top
   (lambda (stx)
     (syntax-case stx (else listof:)
       ;;        [(_  expr
       ;;            clause ...)
       ;;         (with-syntax ([(_ . rest) stx])
       ;;           #'(listof-type-case . rest))]
       [(_ (listof: t) expr
           clause ...)
        (with-syntax ([(_ . rest) stx])
          #'(listof-type-case . rest))]
       [(_ aliased expr
           clause ...)
        ;; handle type alias for `(listof: t)`
        (let loop ([aliased #'aliased] [seen null])
          (define id (if (identifier? aliased)
                         aliased
                         (syntax-case aliased ()
                           [(id arg ...) #'id])))
          (when (for/or ([seen-id (in-list seen)])
                  (free-identifier=? seen-id id))
            (raise-syntax-error #f
                                "recursively defined type alias"
                                id))
          (define val (syntax-local-value id (lambda () #f)))
          (cond
            [(type-alias? val) (loop (type-alias-rhs val) (cons id seen))]
            [else (free-identifier=? id #'listof:)]))        
        (with-syntax ([(_ . rest) stx])
          #'(listof-type-case . rest))]
       ;; Type case without else
       [(_ type expr [(variant id ...) ans] ...)
        (with-syntax ([type (let loop ([type #'type])
                              (define type-id
                                (if (identifier? type)
                                    type
                                    (syntax-case type (Optionof)
                                      [(Optionof arg ...)
                                       lazy?
                                       #'lazy-Optionof]
                                      [(id arg ...) #'id])))
                              (define val (syntax-local-value type-id (lambda () #f)))
                              (if (type-alias? val)
                                  (loop (type-alias-rhs val))
                                  type-id))]
                      [(clause ...) (convert-clauses stx)]
                      [type-case (if lazy? #'lazy:type-case #'type-case)])
          (syntax/loc stx
            (type-case type expr clause ...)))]
       ;; Type case with else
       [(_ type expr [(variant id ...) ans] ... [else else-ans])
        (with-syntax ([type (if (identifier? #'type)
                                #'type
                                (syntax-case #'type ()
                                  [(id arg ...) #'id]))]
                      [(clause ...) (convert-clauses stx)]
                      [type-case (if lazy? #'lazy:type-case #'type-case)])
          (syntax/loc stx
            (type-case type expr clause ...)))]
       ;; Ill-formed typecase
       [_
        (signal-typecase-syntax-error stx)]))))


(define-syntax (empty: stx)
  (define stx-pos (syntax-position stx))
  (syntax-parse stx
    [(_)
     #'(list ) ]
    [_:id
     ;(log-stx stx "ID case")
     #'(list ) ]
    [_ (error "Type Error: empty takes no arguments")]
    ))

(define-syntax (listof-type-case stx)
  (define (clause-kind clause)
    (syntax-case clause (else empty: cons:)
      [[else . _] 'else]
      [[empty: . _] 'empty:]
      [[(empty:) . _] 'empty:]
      [[(cons: id1 id2) . _]
       (let ([check-id (lambda (id)
                         (unless (identifier? id)
                           (raise-syntax-error #f
                                               "expected an identifier for `cons`"
                                               stx
                                               id)))])
         (check-id #'id1)
         (check-id #'id2)
         (when (bound-identifier=? #'id1 #'id2)
           (raise-syntax-error #f
                               "duplicate binding variable for `cons`"
                               stx
                               #'id2))
         'cons)]
      [else
       (raise-syntax-error #f
                           "bad clause"
                           stx
                           clause)]))
  (syntax-case stx ()
    [(_ _ expr
        clause ...)
     (let ([clauses (syntax->list #'(clause ...))])
       (define done
         (for/fold ([done '()]) ([clause (in-list clauses)]
                                 [pos (in-naturals)])
           (define kind (clause-kind clause))
           (when (and (eq? kind 'else)
                      (pos . < . (sub1 (length clauses))))
             (raise-syntax-error #f "`else` clause not at end" stx clause))
           (when (memq kind done)
             (raise-syntax-error #f "variant already covered by a previous clause" stx clause))
           (syntax-case clause ()
             [[_ ans] (void)]
             [_ (raise-syntax-error
                 #f
                 "clause does not contain a single result expression"
                 stx
                 clause)])
           (cons kind done)))
       (cond
         [(or (equal? '(else) done)
              (equal? '(else cons) done)
              (equal? '(else empty:) done)
              (equal? '(cons empty:) done)
              (equal? '(empty: cons) done))
          #`(let ([v expr])
              (#,(if lazy? #'lazy:cond #'cond)
               #,@(for/list ([clause (in-list clauses)])
                    (syntax-case clause (else empty: cons:)
                      [[else ans] clause]
                      [[empty: ans] #'[(#%app: empty? v) ans]]
                      [[(empty:) ans] #'[(#%app: empty? v) ans]]
                      [[(cons: id1 id2) ans]
                       #'[(#%app: pair? v) (let ([id1 (first: v)]
                                                 [id2 (rest: v)])
                                             ans)]]))))]
         [else
          (raise-syntax-error #f
                              (format "missing `~a` clause"
                                      (if (memq 'empty: done) 'cons 'empty))
                              stx)]))]))

(define-syntax cond:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ [ques ans] ...)
        (with-syntax ([(catch ...)
                       (let ([ques (syntax->list #'(ques ...))])
                         (if (and (pair? ques)
                                  (identifier? (last ques))
                                  (free-identifier=? (last ques) #'else))
                             null
                             #`([else #,(syntax/loc stx (cond-error))])))]
                      [cond (if lazy? #'lazy:cond #'cond)])
          (syntax/loc stx
            (cond [ques (#%expression ans)] ... catch ...)))]
       [(_ thing ...)
        (for-each (lambda (thing)
                    (syntax-case thing ()
                      [[ques ans] 'ok]
                      [_else (raise-syntax-error
                              #f
                              "expected [<test-expr> <result-expr>]"
                              stx
                              thing)]))
                  (syntax->list #'(thing ...)))]))))

(define (cond-error)
  (error 'cond "no matching clause"))

(define-syntax case:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ expr [alts ans] ...)
        (with-syntax ([(catch ...)
                       (let loop ([altss (syntax->list #'(alts ...))] [kind #f])
                         (if (null? altss)
                             #'([else (case-error)])
                             (syntax-case (car altss) (else)
                               [else
                                (if (null? (cdr altss))
                                    '()
                                    (raise-syntax-error #f
                                                        "an `else' case must be last"
                                                        stx
                                                        (car altss)))]
                               [(id ...)
                                (let id-loop ([ids (syntax->list #'(id ...))]
                                              [kind kind])
                                  (if (null? ids)
                                      (loop (cdr altss) kind)
                                      (let ([id (car ids)])
                                        (unless (or (identifier? id)
                                                    (number? (syntax-e id)))
                                          (raise-syntax-error #f
                                                              "alternative must be a symbol or a number"
                                                              stx
                                                              id))
                                        (let ([new-kind (if (identifier? id)
                                                            'symbol
                                                            'number)])
                                          (when (and kind (not (eq? new-kind kind)))
                                            (raise-syntax-error #f
                                                                (format "~a disallowed after preceding ~a"
                                                                        new-kind
                                                                        kind)
                                                                stx
                                                                id))
                                          (id-loop (cdr ids) new-kind)))))]
                               [_ (raise-syntax-error #f
                                                      "expected (<id/num> ...)"
                                                      stx
                                                      (car altss))])))]
                      [case (if lazy? #'lazy:case #'case)])
          (syntax/loc stx
            (case expr [alts (#%expression ans)] ... catch ...)))]
       [(_ expr thing ...)
        (for-each (lambda (thing)
                    (syntax-case thing ()
                      [[alts ans] 'ok]
                      [_else (raise-syntax-error
                              #f
                              "expected [(<id/num> ...) <result-expr>] or [else <result-expr>]"
                              stx
                              thing)]))
                  (syntax->list #'(thing ...)))]))))

(define (case-error)
  (error 'case "no matching clause"))

(define-syntax if:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ test then else)
        (with-syntax ([if (if lazy? #'lazy:if #'if)])
          (syntax/loc stx
            (if test then else)))]
       [(_ test then)
        (raise-syntax-error #f
                            "missing else-expression"
                            stx)]))))

(define-syntax when:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ tst expr ...)
        (with-syntax ([when (if lazy? #'lazy:when #'when)])
          (syntax/loc stx (when tst expr ...)))]))))

(define-syntax unless:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ tst expr ...)
        (with-syntax ([unless (if lazy? #'lazy:unless #'unless)])
          (syntax/loc stx (unless tst expr ...)))]))))

(define-for-syntax (check-quoted stx on-escaped)
  (let loop ([s stx] [qq 0])
    (define (fst s) (car (syntax-e s)))
    (define (d->s s e) (datum->syntax s e s s))
    (define (default s)
      (or (and (let ([v (syntax-e s)])
                 (or (symbol? v)
                     (number? v)
                     (string? v)
                     (boolean? v)))
               s)
          (let ([l (syntax->list s)])
            (and l
                 (d->s s (map (lambda (v) (loop v qq)) l))))
          (raise-syntax-error #f
                              "disallowed content; not a symbol, number, boolean, string, or list"
                              stx
                              s)))
    (if on-escaped
        (syntax-case s (unquote unquote-splicing quasiquote:)
          [(unquote e) (if (zero? qq)
                           (on-escaped s)
                           (d->s s `(,(fst s) ,(loop #'e (sub1 qq)))))]
          [(unquote-splicing e) (if (zero? qq)
                                    (on-escaped s)
                                    (d->s s `(,(fst s) ,(loop #'e (sub1 qq)))))]
          [(quasiquote: e) (d->s s `(,(syntax/loc (fst s) quasiquote)
                                     ,(loop #'e (add1 qq))))]
          [unquote (raise-syntax-error #f "bad syntax" s)]
          [unquote-splicing (raise-syntax-error #f "bad syntax" s)]
          [quasiquote: (raise-syntax-error #f "bad syntax" s)]
          [_ (default s)])
        (default s))))

(define-syntax quote:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ s) #'(quote s)]))))

(define-syntax quasiquote:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ s)
        #`(s-exp (quasiquote
                  #,(check-quoted
                     #'s
                     (lambda (stx)
                       (syntax-case stx (unquote unquote-splicing)
                         [(unquote e)(syntax/loc stx
                                       (unquote (s-exp-content e)))]
                         [(unquote-splicing e) (syntax/loc stx
                                                 (unquote-splicing (map s-exp-content e)))])))))]))))
  
(define-syntax and:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ arg ...) (with-syntax ([and (if lazy? #'lazy:and #'and)])
                      (syntax/loc stx (and arg ...)))]))))

(define-syntax or:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ arg ...) (with-syntax ([or (if lazy? #'lazy:or #'or)])
                      (syntax/loc stx (or arg ...)))]))))

(define-syntax set!:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ id expr)
        (if (identifier? #'id)
            (with-syntax ([set! (if lazy? #'lazy:set! #'set!)])
              (syntax/loc stx
                (set! id expr)))
            (raise-syntax-error #f
                                "expected an identifier"
                                stx
                                #'id))]))))

(define-syntax has-type
  (check-top
   (syntax-rules (:)
     [(_ expr : type) expr])))

(define-syntax ....
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(dots arg ...)
        (syntax/loc stx (#%app: (#%expression dots) arg ...))]
       [_ (syntax/loc stx
            (error "reached a `....` placeholder"))]))))

(define-syntax time:
  (check-top
   (syntax-rules ()
     [(_ expr) (time expr)])))

(define-syntax trace:
  (check-top
   (lambda (stx)
     (unless (eq? (syntax-local-context) 'module)
       (raise-syntax-error #f "allowed only module top level" stx))
     (syntax-case stx ()
       [(_ id ...)
        (let ([ids (syntax->list #'(id ...))])
          (for-each (lambda (id)
                      (unless (identifier? id)
                        (raise-syntax-error 'trace
                                            "expected an identifier"
                                            id))
                      (let ([b (identifier-binding id)])
                        (when b
                          (let-values ([(base name) (module-path-index-split (car b))])
                            (when (or base name)
                              (printf "~s\n" (list base name))
                              (raise-syntax-error 'trace
                                                  "not a defined name in this module"
                                                  id))))))
                    ids)
          #'(trace id ...))]))))

(define-syntax: let/cc
  (lambda (stx)
    (syntax-case stx ()
      [(_ id expr)
       (begin
         (unless (identifier? #'id)
           (raise-syntax-error #f "expected an identifier" stx #'id))
         (syntax/loc stx (call/cc (lambda: (id) expr))))])))

;; ----------------------------------------

(define-for-syntax (mk stx . l)
  (datum->syntax
   stx
   (string->symbol
    (apply string-append
           (map (lambda (e)
                  (if (string? e)
                      e
                      (symbol->string (syntax-e e))))
                l)))))

(define-for-syntax (rename-ids ids expr)
  (cond
    [(null? ids) expr]
    [else
     (define d (syntax-local-make-definition-context))
     (syntax-local-bind-syntaxes	ids #f d)
     (internal-definition-context-seal d)
     (internal-definition-context-apply d expr)]))

(define-for-syntax (extract-definition-ids defn)
  (syntax-case defn (: define-type: define: define-values: 
                       define-type-alias)
    [(define-type: name [variant (field-id : field-type) ...] ...)
     (let-values ([(name args)
                   (syntax-case #'name (quote:)
                     [(name (quote arg) ...)
                      (values #'name (syntax->list #'(arg ...)))]
                     [else (values #'name null)])])
       (apply append
              (list #'id)
              (map (lambda (var fields)
                     (list* var
                            (mk var var "?")
                            (map (lambda (field)
                                   (mk var var "-" field))
                                 (syntax->list fields))))
                   (syntax->list #'(variant ...))
                   (syntax->list #'((field-id ...) ...)))))]
    [(define-type-alias (id (quote: arg) ...) t)
     (list #'id)]
    [(define-type-alias id t)
     (list #'id)]
    [(define: (id arg ...) . rest)
     (list #'id)]
    [(define: id : type expr)
     (list #'id)]
    [(define: id expr)
     (list #'id)]
    [(define-values: (id ...) rhs)
     (map (lambda (id)
            (if (identifier? id)
                id
                (syntax-case id ()
                  [[id : type] #'id])))
          (syntax->list #'(id ...)))]
    [_ null]))

;; Since we manage macro expansion during type checking, we're also
;; responsible for renaming at local-binding forms:
(define-for-syntax (rename expr)
  (syntax-case expr (: lambda: local: letrec: let: let*: shared:
                       type-case: TODO) ;;JE
    [(lambda: (arg ...) . _)
     (rename-ids (map (lambda (arg)
                        (syntax-case arg (:)
                          [(id : type) #'id]
                          [else arg]))
                      (syntax->list #'(arg ...))) 
                 expr)]
    [(local: [defn ...] body)
     (rename-ids (apply append
                        (map extract-definition-ids
                             (syntax->list #'(defn ...))))
                 expr)]
    [(letrec: ([id rhs] ...) body)
     (rename-ids (syntax->list #'(id ...)) expr)]
    [(let: ([id rhs] ...) body)
     (rename-ids (syntax->list #'(id ...)) expr)]
    [(let*: ([id rhs] ...) body)
     (rename-ids (syntax->list #'(id ...)) expr)]
    [(shared: ([id rhs] ...) body)
     (rename-ids (syntax->list #'(id ...)) expr)]
    [(type-case: type val clause ...)
     (quasisyntax/loc expr
       (#,(car (syntax-e expr)) type val
                                #,@(map (lambda (clause)
                                          (syntax-case clause ()
                                            [[(variant id ...) ans]
                                             (rename-ids (syntax->list #'(id ...)) clause)]
                                            [_ clause]))
                                        (syntax->list #'(clause ...)))))]
    [_ expr]))

(define-for-syntax (typecheck-defns tl datatypes opaques aliases init-env init-variants just-id? top?
                                    poly-context orig-let-polys submods base-tvars)
  ;; The `base-tvars` is a mapping from type names to type variables
  ;; for type variables written in the source. Since we have to infer
  ;; where type variables are bound, at each binding layer we create a
  ;; fresh box to represent that layer and mutate the box to add
  ;; variables that haven't been seen before, so they're apparently
  ;; bound at the layer represented by the box.
  ;; (displayln (format "Typecheck-defns ~s" (map syntax->datum tl)))
  (let* ([poly-context (cons (gensym) poly-context)]
         [datatypes (append (filter
                             values
                             (map
                              (lambda (stx)
                                (syntax-case stx (define-type:)
                                  [(define-type: (name arg ...) . _) 
                                   (cons #'name (length (syntax->list #'(arg ...))))]
                                  [(define-type: name . _) (cons #'name 0)]
                                  [else #f]))
                              tl))
                            datatypes)]
         [apply-renames (lambda (spec l cdrs-too?)
                          (syntax-case spec (rename-in)
                            [(rename-in spec [old-id new-id] ...)
                             (let ()
                               (define old-ids (syntax->list #'(old-id ...)))
                               (define new-ids (syntax->list #'(new-id ...)))
                               (map (lambda (p)
                                      (let loop ([old-ids old-ids]
                                                 [new-ids new-ids])
                                        (cond
                                          [(null? old-ids) p]
                                          [(free-identifier=? (car old-ids) (car p))
                                           (cons (car new-ids) (cdr p))]
                                          [(and cdrs-too? 
                                                (free-identifier=? (car old-ids) (cdr p)))
                                           (cons (car p) (car new-ids))]
                                          [else (loop (cdr old-ids) (cdr new-ids))])))
                                    l))]))]
         [opaques (append (apply
                           append
                           (map
                            (lambda (stx)
                              (syntax-case stx (require:)
                                [(require: spec ...)
                                 (let loop ([specs (syntax->list #'(spec ...))])
                                   (apply
                                    append
                                    (map (lambda (spec)
                                           (syntax-case spec (opaque-type-in rename-in)
                                             [(opaque-type-in lib [id pred] ...)
                                              (map cons 
                                                   (syntax->list #'(id ...))
                                                   (syntax->list #'(pred ...)))]
                                             [(rename-in sub-spec . _)
                                              (apply-renames spec (loop (list #'sub-spec)) #t)]
                                             [_ null]))
                                         specs)))]
                                [else
                                 null]))
                            tl))
                          opaques)]
         [aliases (append (for/fold ([aliases null]) ([stx (in-list tl)])
                            (syntax-case stx (define-type-alias quote:)
                              [(define-type-alias (name (quote: arg) ...) ty) 
                               (cons (list #'name (syntax->list #'(arg ...)) #'ty)
                                     aliases)]
                              [(define-type-alias name ty) 
                               (cons (list #'name null #'ty) aliases)]
                              [else aliases]))
                          aliases)]
         [expand-alias-application (lambda (t id types seen k)
                                     (ormap (lambda (d)
                                              (and (and (identifier? id)
                                                        (free-identifier=? (car d) id))
                                                   (begin
                                                     (unless (= (length (cadr d)) (length types))
                                                       
                                                       (raise-syntax-error
                                                        #f
                                                        (if (zero? (cdr d))
                                                            "bad type (incorrect use of a non-polymorphic type alias name)"
                                                            "type alias constructor applied to the wrong number of types")
                                                        t))
                                                     (when (ormap (lambda (s)
                                                                    (free-identifier=? s id))
                                                                  seen)
                                                       (raise-syntax-error
                                                        #f
                                                        "recursively defined type alias"
                                                        t))
                                                     
                                                     (k (cadr d)
                                                        (caddr d)
                                                        (cons (car d) seen)))))
                                            aliases))]
         [expand-alias-id (lambda (t seen k)
                            (ormap (lambda (d)
                                     (and (free-identifier=? (car d) t)
                                          (begin
                                            (unless (null? (cadr d))
                                              (raise-syntax-error
                                               #f
                                               "type alias constructor must be applied to types"
                                               t))
                                            (when (ormap (lambda (s)
                                                           (free-identifier=? s t))
                                                         seen)
                                              (raise-syntax-error
                                               #f
                                               "recursively defined type alias"
                                               t))
                                            (k (caddr d) (cons (car d) seen)))))
                                   aliases))]
         [make-polymorphic-wrt
          (lambda (t ty tvars)
            (let loop ([tvars tvars] [ty ty])
              (if (null? tvars)
                  ty
                  (make-poly t
                             (car tvars)
                             (loop (cdr tvars) ty)))))]
         [parse-type/tenv/accum
          (lambda (t tenv tvars/box [rigid? #t])
            (letrec ([parse-one
                      (lambda (seen tenv t)
                        (let loop ([t t])
                          (syntax-case t (number boolean symbol string: char s-expression
                                                 gensym listof: boxof: hashof: parameterof: void: -> 
                                                 vectorof: quote: * Optionof)
                            ;; If we see a type here, then we must be parsing a type the user wrote down
                            ;; So we treat it as rigid until we instantiate the polymorphic function
                            [(quote: id)
                             (identifier? #'id)
                             (let ([a (ormap (lambda (p)
                                               (and (free-identifier=? (car p) #'id)
                                                    p))
                                             (append tenv
                                                     (if (box? tvars/box)
                                                         (unbox tvars/box)
                                                         tvars/box)))])
                               (cond
                                 [a (cdr a)]
                                 [(box? tvars/box)
                                  ;; Give true as the rigid flag
                                  (let ([t (gen-tvar #'id #f rigid?)]
                                        [_ (debugln "Making rigid tvar ~s" #'id)])
                                    (set-box! tvars/box (cons (cons #'id t) (unbox tvars/box)))
                                    t)]
                                 [else
                                  (raise-syntax-error 'define-type-alias "type variable in an alias is not yet in scope" t)]))]
                            [number (make-num t)]
                            [boolean (make-bool t)]
                            [symbol (make-sym t)]
                            [s-expression (make-sexp t)]
                            [string: (make-str t)]
                            [char (make-chr t)]
                            [void: (make-vd t)]
                            [(gensym who) (gen-tvar #'who)]
                            [(arg-type ... -> result-type)
                             (make-arrow t 
                                         (map loop (syntax->list #'(arg-type ...)))
                                         (loop #'result-type))]
                            [(listof: elem)
                             (make-listof t (loop #'elem))]
                            [(boxof: elem)
                             (make-boxof t (loop #'elem))]
                            [(vectorof: elem)
                             (make-vectorof t (loop #'elem))]
                            [(hashof: key val)
                             (make-hashof t (loop #'key) (loop #'val))]
                            [(parameterof: elem)
                             (make-parameterof t (loop #'elem))]
                            [(a * more ...)
                             (let ([m (syntax->list #'(more ...))])
                               (let loop ([m m])
                                 (cond
                                   [(null? m) #f]
                                   [(null? (cdr m)) #t]
                                   [(free-identifier=? #'* (cadr m))
                                    (loop (cddr m))])))
                             (make-tupleof t
                                           (let ploop ([m (syntax->list #'(a * more ...))])
                                             (cond
                                               [(null? (cdr m))
                                                (list (loop (car m)))]
                                               [else
                                                (cons (loop (car m))
                                                      (ploop (cddr m)))])))]
                            [() (make-tupleof t null)]
                            [(Optionof type)
                             (make-datatype t #'Optionof (list (loop #'type)))]
                            [(id type0 type ...)
                             (let ([types (syntax->list #'(type0 type ...))])
                               (or (and (identifier? #'id)
                                        (ormap (lambda (d)
                                                 (and (free-identifier=? (car d) #'id)
                                                      (if (= (cdr d) (length types))
                                                          #t
                                                          (raise-syntax-error
                                                           #f
                                                           (if (zero? (cdr d))
                                                               "bad type (incorrect use of a non-polymorphic type name)"
                                                               "type constructor applied to the wrong number of types")
                                                           t))))
                                               datatypes)
                                        (make-datatype t (car (syntax-e t)) (map loop types)))
                                   (and (identifier? #'id)
                                        (ormap (lambda (d)
                                                 (and (free-identifier=? (car d) #'id)
                                                      (if (null? types)
                                                          (make-opaque-datatype
                                                           t
                                                           (car (syntax-e t))
                                                           null
                                                           (cdr d))
                                                          (raise-syntax-error
                                                           #f
                                                           "bad type (incorrect use of a non-polymorphic type name)"
                                                           t))))
                                               opaques))
                                   (expand-alias-application
                                    t #'id types seen
                                    (lambda (formals rhs seen)
                                      (parse-one seen
                                                 (append (map (lambda (formal arg) 
                                                                (cons formal 
                                                                      (loop arg)))
                                                              formals
                                                              types)
                                                         tenv)
                                                 rhs)))
                                   (raise-syntax-error
                                    #f
                                    "bad type"
                                    t)))]
                            [else
                             (or (and (identifier? t)
                                      (ormap (lambda (d)
                                               (and (free-identifier=? (car d) t)
                                                    (if (zero? (cdr d))
                                                        #t
                                                        (raise-syntax-error
                                                         #f
                                                         "type constructor must be applied to types"
                                                         t))))
                                             datatypes)
                                      (make-datatype t t null))
                                 (and (identifier? t)
                                      (ormap (lambda (d)
                                               (and (free-identifier=? (car d) t)
                                                    (make-opaque-datatype
                                                     t
                                                     t
                                                     null
                                                     (cdr d))))
                                             opaques))
                                 (and (identifier? t)
                                      (expand-alias-id t seen
                                                       (lambda (rhs seen)
                                                         (parse-one seen tenv rhs))))
                                 (raise-syntax-error
                                  #f
                                  "bad type"
                                  t))])))])
              (parse-one null tenv t)))]
         [parse-type/tenv
          (lambda (t tenv)
            (define tvars-box (box base-tvars))
            (define ty (parse-type/tenv/accum t tenv tvars-box))
            (make-polymorphic-wrt t ty (map cdr (unbox tvars-box))))]
         [parse-type (lambda (type)
                       (parse-type/tenv type null))]
         [parse-type/accum (lambda (type tvars-box [rigid? #t])
                             (parse-type/tenv/accum type null tvars-box rigid?))]
         ;; No rigid variables in mono type
         [parse-mono-type (lambda (type tvars-box [rigid? #t])
                            (parse-type/tenv/accum type null tvars-box rigid?))]
         [parse-param-type (lambda (tenv tvars/box)
                             (lambda (type)
                               (parse-type/tenv/accum type tenv tvars/box)))]
         [extract-arg-type (lambda (tvars-box)
                             (lambda (arg)
                               (syntax-case arg (:)
                                 [(id : type) (parse-mono-type #'type tvars-box)]
                                 [_ (gen-tvar #'arg)])))]
         [expand-alias (lambda (t)
                         ;; Usually handled by `parse-type` above, but we need a direct expansion
                         ;; to handle `listof:` in `type-case`. Loop to make sure we discover a
                         ;; cyclic alias.
                         (let* ([ret
                                (let loop ([t t] [seen '()])
                           (syntax-case t ()
                             [(id arg-type ...)
                              (let ([arg-types (syntax->list #'(arg-type ...))])
                                (expand-alias-application
                                 t #'id arg-types seen
                                 (lambda (formals rhs seen)
                                   (define new-type
                                     (let subst ([rhs rhs])
                                       (syntax-case rhs (quote:)
                                         [(sub ...)
                                          (with-syntax ([(sub ...) (map subst (syntax->list #'(sub ...)))])
                                            (syntax/loc rhs (sub ...)))]
                                         [(quote: id)
                                          (identifier? #'id)
                                          (or (for/or ([formal (in-list formals)]
                                                       [arg-type (in-list arg-types)])
                                                (and (free-identifier=? rhs formal)
                                                     arg-type))
                                              (raise-syntax-error 'type "undefined type variable" rhs))]
                                         [_ rhs])))
                                   (or (loop new-type seen)
                                       new-type))))]
                             [_
                              (identifier? t)
                              (expand-alias-id
                               t seen
                               (lambda (rhs seen)
                                 (or (loop rhs seen)
                                     rhs)))]))]
                               [_ (debugln "XXX Expand alias ret ~s" ret)])
                           ret))]
         [macros (apply append
                        (map 
                         (lambda (stx)
                           (syntax-case stx (define-syntax: define-syntax-rule:)
                             [(define-syntax: (id . _) . _)
                              (list #'id)]
                             [(define-syntax: id . _)
                              (list #'id)]
                             [(define-syntax-rule: (id . _) . _)
                              (list #'id)]
                             [_ null]))
                         tl))]
         [variants (apply append
                          init-variants
                          (map 
                           (lambda (stx)
                             (syntax-case stx (define-type: :)
                               [(define-type: name
                                  [variant (field-id : type) ...] ...)
                                (let-values ([(name args)
                                              (syntax-case #'name (quote:)
                                                [(name (quote arg) ...)
                                                 (values #'name (syntax->list #'(arg ...)))]
                                                [else (values #'name null)])])
                                  (let ([arg-types (map gen-tvar args)]
                                        [tvars-box (box base-tvars)])
                                    (map (lambda (variant types)
                                           (cons variant
                                                 (map (lambda (te)
                                                        (make-polymorphic-wrt
                                                         variant
                                                         ((parse-param-type (map cons args arg-types) tvars-box) te)
                                                         arg-types))
                                                      (syntax->list types))))
                                         (syntax->list #'(variant ...))
                                         (syntax->list #'((type ...) ...)))))]
                               [else null]))
                           tl))]
         [is-value? (lambda (expr)
                      (let loop ([expr expr])
                        (syntax-case expr (lambda: list: values: cons: empty hash: quote: none: some: TODO)
                          [(lambda: . _) #t]
                          [(values: a ...)
                           (andmap loop (syntax->list #'(a ...)))]
                          [(list: a ...)
                           (andmap loop (syntax->list #'(a ...)))]
                          [(hash: a ...)
                           (andmap loop (syntax->list #'(a ...)))]
                          [empty #t]
                          [(cons: a b)
                           (and (loop #'a) (loop #'b))]
                          [(id a ...)
                           (and (identifier? #'id)
                                (ormap (lambda (v)
                                         (free-identifier=? #'id (car v)))
                                       variants))
                           (andmap loop (syntax->list #'(a ...)))]
                          [(none:) #t]
                          [(some: e) (loop #'e)]
                          [(quote: a) #t]
                          [_ (or (identifier? expr)
                                 (string? (syntax-e expr))
                                 (char? (syntax-e expr))
                                 (number? (syntax-e expr))
                                 (boolean? (syntax-e expr)))])))]
         [req-env (apply
                   append
                   (map
                    (lambda (stx)
                      (syntax-case stx (require:)
                        [(require: spec ...)
                         (let loop ([specs (syntax->list #'(spec ...))])
                           (apply
                            append
                            (map (lambda (spec)
                                   (syntax-case spec (typed-in rename-in :)
                                     [(typed-in lib (id : type) ...)
                                      (map (lambda (id type)
                                             (cons id (parse-type type)))
                                           (syntax->list #'(id ...))
                                           (syntax->list #'(type ...)))]
                                     [(rename-in sub-spec . _)
                                      (apply-renames spec (loop (list #'sub-spec)) #f)]
                                     [_ null]))
                                 specs)))]
                        [else
                         null]))
                    tl))]
         [def-env (apply
                   append
                   (map
                    (lambda (stx)
                      (syntax-case stx (require: define: define-values: define-type: lambda: TODO :)
                        [(define-values: (id ...) rhs)
                         (let ([val? (is-value? #'rhs)]
                               [tvars-box (box base-tvars)])
                           (map (lambda (id)
                                  (if (identifier? id)
                                      (cons id (if val?
                                                   (create-defn
                                                    (gen-tvar id)
                                                    poly-context
                                                    base-tvars
                                                    (unbox tvars-box))
                                                   (as-non-poly 
                                                    (gen-tvar id)
                                                    poly-context
                                                    (unbox tvars-box))))
                                      (syntax-case id (:)
                                        [(id : type)
                                         (cons #'id 
                                               (if val?
                                                   (create-defn
                                                    (parse-type/accum #'type tvars-box)
                                                    poly-context
                                                    base-tvars
                                                    (unbox tvars-box))
                                                   (as-non-poly
                                                    (parse-mono-type #'type tvars-box)
                                                    poly-context
                                                    (unbox tvars-box))))])))
                                (syntax->list #'(id ...))))]
                        [(define: (id . args) : result-type . _body)
                         (let ([tvars-box (box base-tvars)])
                           (list (cons #'id
                                       (create-defn
                                        (make-arrow 
                                         #'id
                                         (map (extract-arg-type tvars-box)
                                              (syntax->list #'args))
                                         (parse-mono-type #'result-type tvars-box))
                                        poly-context
                                        base-tvars
                                        (unbox tvars-box)))))]
                        [(define: (id . args) . _body)
                         (let ([tvars-box (box base-tvars)])
                           (list (cons #'id (create-defn (make-arrow 
                                                          #'id
                                                          (map (extract-arg-type tvars-box) (syntax->list #'args))
                                                          (gen-tvar #'id))
                                                         poly-context
                                                         base-tvars
                                                         (unbox tvars-box)))))]
                        [(define: id : type (lambda: . _))
                         (let ([tvars-box (box base-tvars)])
                           (list (cons #'id
                                       (create-defn (parse-type/accum #'type tvars-box)
                                                    poly-context
                                                    base-tvars
                                                    (unbox tvars-box)))))]
                        [(define: id (lambda: args : result-type expr))
                         (let ([tvars-box (box base-tvars)])
                           (list (cons #'id
                                       (create-defn
                                        (make-arrow
                                         #'id
                                         (map (extract-arg-type tvars-box) (syntax->list #'args))
                                         (parse-mono-type #'result-type tvars-box))
                                        poly-context
                                        base-tvars
                                        (unbox tvars-box)))))]
                        [(define: id (lambda: args expr))
                         (let ([tvars-box (box base-tvars)])
                           (list (cons #'id
                                       (create-defn
                                        (make-arrow
                                         #'id
                                         (map (extract-arg-type tvars-box) (syntax->list #'args))
                                         (gen-tvar #'id))
                                        poly-context
                                        base-tvars
                                        (unbox tvars-box)))))]
                        [(define: id : type expr)
                         (let ([tvars-box (box base-tvars)])
                           (list (cons #'id 
                                       (if (is-value? #'expr)
                                           (create-defn
                                            (parse-type/accum #'type tvars-box)
                                            poly-context
                                            base-tvars
                                            (unbox tvars-box))
                                           (as-non-poly
                                            (parse-mono-type #'type tvars-box)
                                            poly-context
                                            (unbox tvars-box))))))]
                        [(define: id expr)
                         (list (cons #'id 
                                     (if (is-value? #'expr)
                                         (create-defn
                                          (gen-tvar #'id)
                                          poly-context
                                          base-tvars
                                          null)
                                         (as-non-poly
                                          (gen-tvar #'id)
                                          poly-context
                                          null))))]
                        [(define-type: name
                           [variant (field-id : type) ...]
                           ...)
                         (let-values ([(name args)
                                       (syntax-case #'name (quote:)
                                         [(name (quote arg) ...)
                                          (values #'name (syntax->list #'(arg ...)))]
                                         [else (values #'name null)])])
                           (let ([arg-tvars (map gen-tvar args)]
                                 [tvars-box (box base-tvars)])
                             (apply append
                                    (map (lambda (var fields types)
                                           (let ([types (map (parse-param-type 
                                                              (map cons args arg-tvars)
                                                              tvars-box)
                                                             (syntax->list types))]
                                                 [dt (make-datatype name 
                                                                    name
                                                                    arg-tvars)])
                                             (list* (cons var
                                                          (make-polymorphic-wrt
                                                           var
                                                           (make-arrow
                                                            var
                                                            types
                                                            dt)
                                                           arg-tvars))
                                                    (cons (mk var var "?")
                                                          (make-polymorphic-wrt
                                                           var
                                                           (make-arrow
                                                            var
                                                            (list dt)
                                                            (make-bool var))
                                                           arg-tvars))
                                                    (map (lambda (field type)
                                                           (cons (mk var var "-" field)
                                                                 (make-polymorphic-wrt
                                                                  field
                                                                  (make-arrow
                                                                   field
                                                                   (list dt)
                                                                   type)
                                                                  arg-tvars)))
                                                         (syntax->list fields)
                                                         types))))
                                         (syntax->list #'(variant ...))
                                         (syntax->list #'((field-id ...) ...))
                                         (syntax->list #'((type ...) ...))))))]
                        [else null]))
                    tl))]
         [ext-def-env
          (if (not top?)
              def-env
              ;; Add any identifiers that have just a type declaration:
              (for/fold ([def-env def-env]) ([stx (in-list tl)])
                (syntax-case stx (:)
                  [(id : type)
                   (and (identifier? #'id)
                        (not (lookup #'id def-env #:default #f))
                        (not (lookup #'id init-env #:default #f)))
                   (cons (cons #'id (parse-type/accum #'type (box base-tvars)))
                         def-env)]
                  [_ def-env])))]
         [def-env (if (not top?)
                      ext-def-env
                      ;; If any name is already defined in `init-env`, keep
                      ;; old type
                      (for/fold ([def-env null]) ([p (in-list ext-def-env)])
                        (cond
                          [(lookup (car p) init-env #:default #f)
                           => (lambda (t)
                                (unify-defn! (car p) (cdr p) (poly-instance t))
                                (debugln "Adding 2 to env ~s   and ~s" (car p) t)
                                (cons (cons (car p) t) def-env))]
                          [else (cons p def-env)])))]
         [env (append def-env
                      req-env
                      init-env)]
         [start-env env]
         [let-polys (or orig-let-polys (box null))]
         ;; typecheck the sequence:
         [types
          (map
           (lambda (tl)
             (let typecheck ([expr tl] [env env] [tvars-box (box base-tvars)])
               (debugln (format "Typecheck ~s \ntoplevel ~s\n" (syntax->datum (rename expr)) tl))
               (let ([ret (syntax-case (rename expr) (: begin require: define-type: define: define-values:
                                                        define-type-alias define-syntax: define-syntax-rule:
                                                        lambda: begin: local: letrec: let: let*: TODO ;;JE
                                                        shared: parameterize:
                                                        begin: cond: case: if: when: unless:
                                                        or: and: set!: trace:
                                                        type-case: quote: quasiquote: time: listof:
                                                        else empty empty:
                                                        has-type ....
                                                        list: vector: values: try
                                                        module+: module)
                            [TODO (gen-tvar exp)] ;JE
                            [(module+: name e ...)
                             (let*-values ([(datatypes dt-len opaques o-len aliases a-len
                                                       variants v-len env e-len
                                                       prev-macros prev-tys prev-tl-tys prev-submods)
                                            (vector->values (hash-ref submods (syntax-e #'name)
                                                                      (vector datatypes
                                                                              (length datatypes)
                                                                              opaques
                                                                              (length opaques)
                                                                              aliases
                                                                              (length aliases)
                                                                              variants
                                                                              (length variants)
                                                                              env
                                                                              (length env)
                                                                              null ; macros
                                                                              null ; tys
                                                                              null ; tl-tys
                                                                              (hasheq))))]
                                           [(tys env datatypes opaques aliases variants macros tl-tys next-submods)
                                            (typecheck-defns (syntax->list #'(e ...))
                                                             datatypes
                                                             opaques
                                                             aliases
                                                             env
                                                             variants
                                                             #f
                                                             #f ; not top
                                                             poly-context
                                                             let-polys
                                                             prev-submods
                                                             base-tvars)])
                               (set! submods (hash-set submods (syntax-e #'name)
                                                       (vector datatypes dt-len
                                                               opaques o-len
                                                               aliases a-len
                                                               variants v-len
                                                               env e-len
                                                               (append macros prev-macros)
                                                               (append tys prev-tys)
                                                               (append tl-tys prev-tl-tys)
                                                               next-submods))))]
                            [(module . _)
                             ;; can ignore
                             (void)]
                            [(define-syntax: . _)
                             ;; can ignore
                             (void)]
                            [(define-syntax-rule: . _)
                             ;; can ignore
                             (void)]
                            [(begin . _)
                             ;; can ignore macro-introduced
                             (void)]
                            [(require: . _)
                             ;; handled in require env
                             (void)]
                            [(define-type: id [variant (field-id : field-type) ...] ...)
                             ;; handled in initial env
                             (void)]
                            [(define-type-alias (id (quote: arg) ...) t)
                             ;; check that `t' makes sense
                             ((parse-param-type (map (lambda (arg) (cons arg (gen-tvar arg)))
                                                     (syntax->list #'(arg ...)))
                                                ;; Not a box => no free type variables allowed
                                                base-tvars)
                              #'t)
                             (void)]
                            [(define-type-alias id t)
                             ;; check that `t' makes sense
                             ((parse-param-type null base-tvars) #'t)
                             (void)]
                            [(id : type)
                             (let ([id #'id])
                               (unless (identifier? id)
                                 (raise-syntax-error 'declaration "expected an identifier before `:`" id))
                               (unless (or top? (lookup id def-env #:default #f))
                                 (raise-syntax-error 'declaration "identifier not defined here" id))
                               (unify! expr
                                       (poly-instance (lookup id env))
                                       (parse-type/accum #'type (box (unbox tvars-box)))))]
                            [(id : . _)
                             (if (identifier? #'id)
                                 (raise-syntax-error 'declaration "expected a single type after `:`" expr)
                                 (raise-syntax-error 'declaration "expected an identifier before `:`" #'id))]
                            [(define: (id arg ...) . rest)
                             (typecheck (with-syntax ([proc (syntax/loc expr (lambda: (arg ...) . rest))])
                                          (syntax/loc expr (define: id proc)))
                                        env
                                        tvars-box)]
                            [(define: id : type expr)
                             (let ([dt (lookup #'id env)])
                               ;; Note: if we're at the top level and `dt` is a polymorphic type
                               ;; from a previous interaction, we won't be able to redefine, because
                               ;; `unify!` cannot unify polymorphic types
                               (unify-defn! #'expr dt
                                            (typecheck #'expr env (box (get-defn-tvars dt)))))]
                            [(define: id expr)
                             (typecheck #'(define: id : (gensym id) expr)
                                        env
                                        tvars-box)]
                            [(define-values: (id ...) rhs)
                             (let ([id-ids (map (lambda (id)
                                                  (if (identifier? id)
                                                      id
                                                      (car (syntax-e id))))
                                                (syntax->list #'(id ...)))]
                                   [id-types (map (lambda (id)
                                                    (syntax-case id (:)
                                                      [(id : type)
                                                       (poly-instance (parse-type/accum #'type tvars-box))]
                                                      [else (gen-tvar id)]))
                                                  (syntax->list #'(id ...)))])
                               (unify! expr
                                       (make-tupleof expr id-types)
                                       (typecheck #'rhs env tvars-box))
                               (for-each (lambda (id tvar)
                                           (unify-defn! expr
                                                        (lookup id env)
                                                        tvar))
                                         id-ids
                                         id-types))]
                
                            [(lambda: (arg ...) : type body)
                             (let ([tvars-box (box (unbox tvars-box))])
                               (let ([arg-ids (map (lambda (arg)
                                                     (if (identifier? arg)
                                                         arg
                                                         (car (syntax-e arg))))
                                                   (syntax->list #'(arg ...)))]
                                     [arg-types (map (lambda (arg)
                                                       (syntax-case arg (:)
                                                         [(id : type)
                                                          (poly-instance (parse-type/accum #'type tvars-box))]
                                                         [else (gen-tvar arg)]))
                                                     (syntax->list #'(arg ...)))]
                                     [result-type (poly-instance (parse-type/accum #'type tvars-box))])
                                 (unify! #'body
                                         (typecheck #'body (append (map cons 
                                                                        arg-ids
                                                                        arg-types)
                                                                   env)
                                                    tvars-box)
                                         result-type)
                                 (make-arrow expr arg-types result-type)))]
                            [(lambda: (arg ...) body)
                             (with-syntax ([expr expr])
                               (typecheck (syntax/loc #'expr
                                            (lambda: (arg ...) : (gensym expr) body))
                                          env
                                          tvars-box))]
                            [(begin: e ... last-e)
                             (begin
                               (map (lambda (e)
                                      (typecheck e env tvars-box))
                                    (syntax->list #'(e ...)))
                               (typecheck #'last-e env tvars-box))]
                            [(local: [defn ...] expr)
                             (let-values ([(ty env datatypes opaques aliases vars macros tl-tys subs)
                                           (typecheck-defns (syntax->list #'(defn ...))
                                                            datatypes
                                                            opaques
                                                            aliases
                                                            env
                                                            variants
                                                            #f
                                                            #f ; not top
                                                            poly-context
                                                            let-polys
                                                            submods
                                                            (unbox tvars-box))])
                               (typecheck #'expr env tvars-box))]
                            [(letrec: . _)
                             (typecheck ((make-let 'letrec) expr) env tvars-box)]
                            [(let: . _)
                             (typecheck ((make-let 'let) expr) env tvars-box)]
                            [(let*: . _)
                             (let* ([made-let ((make-let 'let*) expr)]
                                    [_ (debugln "made-let: ~s" made-let)])
                               (typecheck made-let env tvars-box))]
                            [(shared: ([id rhs] ...) expr)
                             (let-values ([(ty env datatypes opaques aliases vars macros tl-tys subs)
                                           (typecheck-defns (syntax->list #'((define: id rhs) ...))
                                                            datatypes
                                                            opaques
                                                            aliases
                                                            env
                                                            variants
                                                            #f
                                                            #f ; not top
                                                            poly-context
                                                            let-polys submods
                                                            (unbox tvars-box))])
                               (typecheck #'expr env tvars-box))]
                            [(parameterize: ([param rhs] ...) expr)
                             (begin
                               (for ([param (in-list (syntax->list #'(param ...)))]
                                     [rhs (in-list (syntax->list #'(rhs ...)))])
                                 (unify! #'param 
                                         (typecheck param env tvars-box)
                                         (make-parameterof rhs (typecheck rhs env tvars-box))))
                               (typecheck #'expr env tvars-box))]
                            [(cond: [ques ans] ...)
                             (let ([res-type (gen-tvar expr)])
                               (for-each
                                (lambda (ques ans)
                                  (unless (syntax-case ques (else)
                                            [else #t]
                                            [_ #f])
                                    (unify! ques
                                            (make-bool ques)
                                            (typecheck ques env tvars-box)))
                                  (unify! ans
                                          res-type
                                          (typecheck ans env tvars-box)))
                                (syntax->list #'(ques ...))
                                (syntax->list #'(ans ...)))
                               res-type)]
                            [(case: expr [alts ans] ...)
                             (let ([res-type (gen-tvar #'expr)])
                               (unify! #'expr
                                       (let loop ([alts (syntax->list #'(alts ...))])
                                         (if (null? alts)
                                             (make-sym #'expr)
                                             (syntax-case (car alts) ()
                                               [() (loop (cdr alts))]
                                               [(v . _)
                                                (number? (syntax-e #'v))
                                                (make-num #'expr)]
                                               [_ (make-sym #'expr)])))
                                       (typecheck #'expr env tvars-box))
                               (for-each
                                (lambda (ans)
                                  (unify! #'ans
                                          res-type
                                          (typecheck ans env tvars-box)))
                                (syntax->list #'(ans ...)))
                               res-type)]
                            [(if: test then els)
                             (begin
                               (unify! #'test
                                       (make-bool #'test)
                                       (typecheck #'test env tvars-box))
                               (let ([then-type (typecheck #'then env tvars-box)])
                                 (unify! #'then then-type (typecheck #'els env tvars-box))
                                 then-type))]
                            [(when: test e ...)
                             (begin
                               (unify! #'test
                                       (make-bool #'test)
                                       (typecheck #'test env tvars-box))
                               (typecheck #'(begin: e ...) env tvars-box)
                               (make-vd expr))]
                            [(unless: test e ...)
                             (begin
                               (unify! #'test
                                       (make-bool #'test)
                                       (typecheck #'test env tvars-box))
                               (typecheck #'(begin: e ...) env tvars-box)
                               (make-vd expr))]
                            [(and: e ...)
                             (let ([b (make-bool expr)])
                               (for-each (lambda (e)
                                           (unify! e b (typecheck e env tvars-box)))
                                         (syntax->list #'(e ...)))
                               b)]
                            [(or: e ...)
                             (let ([b (make-bool expr)])
                               (for-each (lambda (e)
                                           (unify! e b (typecheck e env tvars-box)))
                                         (syntax->list #'(e ...)))
                               b)]
                            [(set!: id e)
                             (let ([t (lookup #'id env)])
                               (if (poly? t)
                                   (raise-syntax-error #f
                                                       "cannot mutate identifier with a polymorphic type"
                                                       expr
                                                       #'id)
                                   (unify-defn! #'id t (typecheck #'e env tvars-box))))
                             (make-vd expr)]
                            [(trace: id ...)
                             (let ([ids (syntax->list #'(id ...))])
                               (for-each (lambda (id)
                                           (unify! id (gen-tvar id #t) (typecheck id env tvars-box)))
                                         ids)
                               (make-tupleof expr null))]
                 
                            [(type-case: (listof: elem-type) val clause ...)
                             ;; special handling for `listof` case
                             (syntax-case expr ()
                               [(_ type . _)
                                ;; Don't do rigid here, since this is just to help plai stuff
                                (let* ([elem-type (parse-type/accum #'elem-type tvars-box #f)]
                                       [type (make-listof #'type elem-type)]
                                       [res-type (gen-tvar expr)])
                                  (unify! #'val type (typecheck #'val env tvars-box))
                                  (for-each (lambda (clause)
                                              (syntax-case clause (cons:)
                                                [[(cons: id1 id2) ans]
                                                 (unify!
                                                  expr
                                                  res-type
                                                  (typecheck #'ans 
                                                             (cons (cons #'id1 elem-type)
                                                                   (cons (cons #'id2 type)
                                                                         env))
                                                             tvars-box))]
                                                [[_ ans]
                                                 (unify!
                                                  expr
                                                  res-type
                                                  (typecheck #'ans env tvars-box))]))
                                            (syntax->list #'(clause ...)))
                                  res-type)])]
                            [(type-case: type val [(variant id ...) ans] ...)
                             ;; Don't do rigid here, since this is redundant with inference
                             (let ([type (parse-mono-type #'type tvars-box #f)]
                                   [res-type (gen-tvar expr)])
                               (unify! #'val type (typecheck #'val env tvars-box))
                               (for-each (lambda (var ids ans)
                                           (let ([id-lst (syntax->list ids)]
                                                 [variant-params (lookup var variants)])
                                             (unless (= (length id-lst)
                                                        (length variant-params))
                                               (raise-syntax-error 'type-case
                                                                   (format "variant ~a has ~a fields in the definition but ~a fields here at a use"
                                                                           (syntax-e var)
                                                                           (length variant-params)
                                                                           (length id-lst))
                                                                   var))
                                             (unify!
                                              expr
                                              res-type
                                              (typecheck ans
                                                         (append (map (lambda (id ftype)
                                                                        (cons id
                                                                              (instantiate-constructor-at
                                                                               ftype
                                                                               type)))
                                                                      id-lst
                                                                      variant-params)
                                                                 env)
                                                         tvars-box))))
                                         (syntax->list #'(variant ...))
                                         (syntax->list #'((id ...) ...))
                                         (syntax->list #'(ans ...)))
                               res-type)]
                            [(type-case: type val [(variant id ...) ans] ... [else else-ans])
                             (let ([t (typecheck (syntax/loc expr
                                                   (type-case: type val [(variant id ...) ans] ...))
                                                 env
                                                 tvars-box)])
                               (unify! #'else-ans t (typecheck #'else-ans env tvars-box))
                               t)]
                            [(type-case: type val [(variant id ...) ans] ... [empty else-ans])
                             (typecheck (syntax/loc expr
                                          (type-case: type val [(variant id ...) ans] ... [(empty-list) else-ans]))
                                        env
                                        tvars-box)]
                            [(type-case: type val clause ...)
                             ;; a type alias can expand to `Listof`, so catch that possibility here
                             (let* ([new-type (expand-alias #'type)]
                                    [_ (debugln "XXX Expanded ~s  into ~s" #'type new-type)])
                               (cond
                                 [new-type
                                  (syntax-case expr ()
                                    [(tc . _)
                                     (with-syntax ([new-type new-type])
                                       (typecheck (syntax/loc expr
                                                    (tc new-type val clause ...))
                                                  env
                                                  tvars-box))])]
                                 [else (signal-typecase-syntax-error expr)]))]
                            [(type-case: . rest)
                             (signal-typecase-syntax-error expr)]
                            [(quote: e)
                             (let ([orig-expr expr])
                               (let loop ([e #'e] [expr expr])
                                 (define v (syntax-e e))
                                 (cond
                                   [(symbol? v) (make-sym expr)]
                                   [(number? v) (make-num expr)]
                                   [(boolean? v) (make-bool expr)]
                                   [(string? v) (make-str expr)]
                                   [(null? v) (make-listof expr (gen-tvar expr))]
                                   [(pair? v)
                                    (define hd (make-listof expr (loop (car v) (car v))))
                                    (unify! expr hd (loop (datum->syntax e (cdr v)) expr))
                                    hd]
                                   [(vector? v)
                                    (define t (gen-tvar expr))
                                    (for ([e (in-vector v)])
                                      (unify! expr (loop e e) t))
                                    (make-vectorof expr t)]
                                   [(box? v)
                                    (make-boxof expr (loop (unbox v) (unbox v)))]
                                   [else (raise-syntax-error
                                          #f
                                          "disallowed content; not a symbol, number, boolean, string, list, vector, or box"
                                          orig-expr
                                          e)])))]
                            [(quasiquote: e)
                             (check-quoted #'e (lambda (stx)
                                                 (syntax-case stx (unquote unquote-splicing)
                                                   [(unquote e) (unify! #'e 
                                                                        (typecheck #'e env tvars-box)
                                                                        (make-sexp #f))]
                                                   [(unquote-splicing e) (unify! #'e 
                                                                                 (typecheck #'e env tvars-box) 
                                                                                 (make-listof #f (make-sexp #f)))])))
                             (make-sexp expr)]
                            [(time: expr)
                             (typecheck #'expr env tvars-box)]
                            [(has-type expr : type)
                             (let ([t (typecheck #'expr env tvars-box)]
                                   [ty (parse-mono-type #'type tvars-box)])
                               (unify! #'expr t ty)
                               ty)]
                            [....
                             (gen-tvar expr)]
                            [(try expr1 (lambda: () expr2))
                             (let ([t (typecheck #'expr1 env tvars-box)])
                               (unify! #'expr2 t (typecheck #'expr2 env tvars-box))
                               t)]

                            [(empty:)
                             (let ([t (gen-tvar expr)])
                               (make-listof expr t))]
                            [(list: arg ...)
                             (let ([t (gen-tvar expr)])
                               (for-each (lambda (arg)
                                           (unify! arg t (typecheck arg env tvars-box)))
                                         (syntax->list #'(arg ...)))
                               (make-listof expr t))]
                            [list:
                             (raise-syntax-error #f
                                                 "list constructor must be applied directly to arguments"
                                                 expr)]
                            [(vector: arg ...)
                             (let ([t (gen-tvar expr)])
                               (for-each (lambda (arg)
                                           (unify! arg t (typecheck arg env tvars-box)))
                                         (syntax->list #'(arg ...)))
                               (make-vectorof expr t))]
                            [vector:
                             (raise-syntax-error #f
                                                 "vector constructor must be applied directly to arguments"
                                                 expr)]
                            [(values: arg ...)
                             (make-tupleof expr
                                           (map (lambda (arg)
                                                  (typecheck arg env tvars-box))
                                                (syntax->list #'(arg ...))))]
                            [values:
                             (raise-syntax-error #f
                                                 "tuple constructor must be applied directly to arguments"
                                                 expr)]
                 
                            [(id . _)
                             (and (identifier? #'id)
                                  (typed-macro? (syntax-local-value #'id (lambda () #f))))
                             (typecheck (local-expand-typed expr) env tvars-box)]
                            [(f arg ...)
                             (debugln "APP")
                             (let* ([res-type (gen-tvar expr)]
                                    [_ (debugln "Checking application ~s to ~s" #'f #'(arg ...))])
                               (unify! #'f
                                       (typecheck #'f env tvars-box)
                                       (make-arrow #'f
                                                   (map (lambda (arg)
                                                          (typecheck arg env tvars-box))
                                                        (syntax->list #'(arg ...)))
                                                   res-type)
                                       #:function-call? #t)
                               res-type)]
                            [_else
                             (cond
                               [(identifier? expr)
                                (let* ([t (lookup expr env)]
                                       [ret (if just-id?
                                                t
                                                (at-source (poly-instance t) expr))])
                                  (begin
                                    (debugln "Getting type for identifier ~s ~n ~s\ninst: ~s" expr (~a t #:max-width +inf.0)  ret)
                                    ret))]
                               [(boolean? (syntax-e expr))
                                (make-bool expr)]
                               [(number? (syntax-e expr))
                                (make-num expr)]
                               [(string? (syntax-e expr))
                                (make-str expr)]
                               [(char? (syntax-e expr))
                                (make-chr expr)]
                               [(eq? (void) (syntax-e expr))
                                (void)]
                               [else
                                (raise-syntax-error #f
                                                    "don't know how to typecheck"
                                                    expr)])])])
                 (begin
                   (define num-locals (- (length env) (length start-env)))
                   (define local-env
                     (take env num-locals))
                   (debugln "XX: Recording type ~s for pos ~s\n     locals ~s ~n IGNORED: ~s" (pretty-type ret) (syntax-position expr) local-env mutable-ignored-vars)
                   (hash-set! types-for-locs (syntax-position expr) (cons ret local-env))
                   ret))))
           tl)])
    (set-box! let-polys (cons def-env (unbox let-polys)))
    (define poly-env
      (begin
        (debugln "POLY ENV")
        (if orig-let-polys
            def-env
            (let-based-poly! (apply append (unbox let-polys)) fuel))))
    (define poly-def-env
      (if (eq? poly-env def-env)
          def-env
          (take poly-env (length def-env))))
    (values
     types
     (if (eq? def-env poly-def-env)
         env
         (append poly-def-env
                 req-env
                 init-env))
     datatypes
     opaques
     aliases
     variants
     macros
     poly-def-env
     submods)))

(define-for-syntax tl-env #f)
(define-for-syntax tl-datatypes #f)
(define-for-syntax tl-opaques #f)
(define-for-syntax tl-aliases #f)
(define-for-syntax tl-variants #f)
(define-for-syntax tl-submods #f)

(define-for-syntax (do-original-typecheck tl)
  ;; (displayln (format "Do-Original-Typecheck ~s" tl))
  (let ([datatypes null]
        [opaques null]
        [aliases null]
        [init-env (let ([NN->N (make-arrow #f 
                                           (list (make-num #f)
                                                 (make-num #f))
                                           (make-num #f))]
                        [N->N (make-arrow #f 
                                          (list (make-num #f))
                                          (make-num #f))]
                        [NN->B (make-arrow #f 
                                           (list (make-num #f)
                                                 (make-num #f))
                                           (make-bool #f))]
                        [N->B (make-arrow #f 
                                          (list (make-num #f))
                                          (make-bool #f))]
                        [N (make-num #f)]
                        [B (make-bool #f)]
                        [STR (make-str #f)]
                        [CHAR (make-chr #f)]
                        [SYM (make-sym #f)]
                        [BOOL (make-bool #f)])
                    (define-syntax-rule (POLY a e)
                      (let ([a (gen-tvar #f)]) (make-poly #f a e)))
                    (list
                     (cons #'error
                           (POLY a
                                 (make-arrow #f
                                             (list SYM
                                                   STR)
                                             a)))
                     (cons #'void
                           (make-arrow #f null (make-vd #f)))
                     (cons #'not
                           (make-arrow #f
                                       (list B)
                                       B))
                     (cons #'+ NN->N)
                     (cons #'- NN->N)
                     (cons #'/ NN->N)
                     (cons #'* NN->N)
                     (cons #'= NN->B)
                     (cons #'< NN->B)
                     (cons #'> NN->B)
                     (cons #'<= NN->B)
                     (cons #'>= NN->B)
                     (cons #'min NN->N)
                     (cons #'max NN->N)
                     (cons #'modulo NN->N)
                     (cons #'remainder NN->N)
                     (cons #'floor N->N)
                     (cons #'ceiling N->N)
                     (cons #'add1 N->N)
                     (cons #'sub1 N->N)
                     (cons #'zero? N->B)
                     (cons #'odd? N->B)
                     (cons #'even? N->B)
                     (cons #'symbol=? (make-arrow #f 
                                                  (list SYM
                                                        SYM)
                                                  B))
                     (cons #'string=? (make-arrow #f 
                                                  (list STR
                                                        STR)
                                                  B))
                     (cons #'char=? (make-arrow #f 
                                                (list CHAR
                                                      CHAR)
                                                B))
                     (cons #'make-hash: (POLY a (POLY b (make-arrow #f
                                                                    (list (make-listof 
                                                                           #f
                                                                           (make-tupleof
                                                                            #f
                                                                            (list a b))))
                                                                    (make-hashof #f a b)))))
                     (cons #'hash: (POLY a (POLY b (make-arrow #f
                                                               (list (make-listof 
                                                                      #f
                                                                      (make-tupleof
                                                                       #f
                                                                       (list a b))))
                                                               (make-hashof #f a b)))))
                     (cons #'hash-ref: (POLY a (POLY b (make-arrow #f
                                                                   (list (make-hashof #f a b)
                                                                         a)
                                                                   (make-datatype #f #'Optionof (list b))))))
                     (cons #'hash-set! (POLY a (POLY b (make-arrow #f
                                                                   (list (make-hashof #f a b)
                                                                         a
                                                                         b)
                                                                   (make-vd #f)))))
                     (cons #'hash-remove! (POLY a (POLY b (make-arrow #f
                                                                      (list (make-hashof #f a b)
                                                                            a)
                                                                      (make-vd #f)))))
                     (cons #'hash-set (POLY a (POLY b (make-arrow #f
                                                                  (list (make-hashof #f a b)
                                                                        a
                                                                        b)
                                                                  (make-hashof #f a b)))))
                     (cons #'hash-remove (POLY a (POLY b (make-arrow #f
                                                                     (list (make-hashof #f a b)
                                                                           a)
                                                                     (make-hashof #f a b)))))
                     (cons #'hash-keys (POLY a (POLY b (make-arrow #f
                                                                   (list (make-hashof #f a b))
                                                                   (make-listof #f a)))))
                     (cons #'s-exp-symbol? (make-arrow #f 
                                                       (list (make-sexp #f))
                                                       B))
                     (cons #'s-exp->symbol (make-arrow #f 
                                                       (list (make-sexp #f))
                                                       SYM))
                     (cons #'symbol->s-exp (make-arrow #f 
                                                       (list SYM)
                                                       (make-sexp #f)))
                     (cons #'s-exp-number? (make-arrow #f 
                                                       (list (make-sexp #f))
                                                       B))
                     (cons #'s-exp->number (make-arrow #f 
                                                       (list (make-sexp #f))
                                                       N))
                     (cons #'number->s-exp (make-arrow #f 
                                                       (list N)
                                                       (make-sexp #f)))
                     (cons #'s-exp-string? (make-arrow #f 
                                                       (list (make-sexp #f))
                                                       B))
                     (cons #'s-exp->string (make-arrow #f 
                                                       (list (make-sexp #f))
                                                       STR))
                     (cons #'string->s-exp (make-arrow #f 
                                                       (list STR)
                                                       (make-sexp #f)))
                     (cons #'s-exp-boolean? (make-arrow #f 
                                                        (list (make-sexp #f))
                                                        B))
                     (cons #'s-exp->boolean (make-arrow #f 
                                                        (list (make-sexp #f))
                                                        BOOL))
                     (cons #'boolean->s-exp (make-arrow #f 
                                                        (list BOOL)
                                                        (make-sexp #f)))
                     (cons #'s-exp-list? (make-arrow #f 
                                                     (list (make-sexp #f))
                                                     B))
                     (cons #'s-exp->list (make-arrow #f 
                                                     (list (make-sexp #f))
                                                     (make-listof #f (make-sexp #f))))
                     (cons #'list->s-exp (make-arrow #f 
                                                     (list (make-listof #f (make-sexp #f)))
                                                     (make-sexp #f)))
                     (cons #'read: (make-arrow #f null (make-sexp #f)))
                     (cons #'s-exp-match? (make-arrow #f 
                                                      (list (make-sexp #f)
                                                            (make-sexp #f))
                                                      B))
                     (cons #'equal? (POLY a (make-arrow #f 
                                                        (list a a)
                                                        B)))
                     (cons #'eq? (POLY a (make-arrow #f 
                                                     (list a a)
                                                     B)))
                     (cons #'test: (POLY a (make-arrow #f 
                                                       (list a a)
                                                       (make-vd #f))))

                     (cons #'test/noerror  (POLY a (make-arrow #f
                                                               (list a)
                                                               (make-vd #f))))
                     (cons #'test/exn: (POLY a (make-arrow #f 
                                                           (list a
                                                                 STR)
                                                           (make-vd #f))))
                     (cons #'print-only-errors (make-arrow #f 
                                                           (list B)
                                                           (make-vd #f)))
                     (cons #'call/cc (POLY a
                                           (POLY b
                                                 (make-arrow #f
                                                             (list (make-arrow
                                                                    #f
                                                                    (list (make-arrow
                                                                           #f
                                                                           (list a)
                                                                           b))
                                                                    a))
                                                             a))))
                     (cons #'empty: (POLY a (make-listof #f a)))
                     (cons #'cons: (POLY a (make-arrow #f
                                                       (list a (make-listof #f a))
                                                       (make-listof #f a))))
                     (cons #'cons? (POLY a (make-arrow #f
                                                       (list (make-listof #f a))
                                                       B)))
                     (cons #'empty? (POLY a (make-arrow #f
                                                        (list (make-listof #f a))
                                                        B)))
                     (cons #'first: (POLY a (make-arrow #f
                                                        (list (make-listof #f a))
                                                        a)))
                     (cons #'rest: (POLY a (make-arrow #f
                                                       (list (make-listof #f a))
                                                       (make-listof #f a))))
                     (cons #'second: (POLY a
                                           (make-arrow #f
                                                       (list (make-listof #f a))
                                                       a)))
                     (cons #'third: (POLY a (make-arrow #f
                                                        (list (make-listof #f a))
                                                        a)))
                     (cons #'fourth: (POLY a (make-arrow #f
                                                         (list (make-listof #f a))
                                                         a)))
                     (cons #'list-ref: (POLY a (make-arrow #f
                                                           (list (make-listof #f a)
                                                                 N)
                                                           a)))
                     (cons #'build-list (POLY a (make-arrow #f
                                                            (list N
                                                                  (make-arrow #f
                                                                              (list N)
                                                                              a))
                                                            (make-listof #f a))))
                     (cons #'length: (POLY a (make-arrow #f
                                                         (list (make-listof #f a))
                                                         N)))
                     (cons #'map: (POLY a
                                        (POLY b
                                              (make-arrow #f
                                                          (list (make-arrow #f (list a) b)
                                                                (make-listof #f a))
                                                          (make-listof #f b)))))
                     (cons #'map2: (POLY a
                                         (POLY b
                                               (POLY c
                                                     (make-arrow #f
                                                                 (list (make-arrow #f (list a b) c)
                                                                       (make-listof #f a)
                                                                       (make-listof #f b))
                                                                 (make-listof #f c))))))
                     (cons #'member: (POLY a (make-arrow #f
                                                         (list a
                                                               (make-listof #f a))
                                                         B)))
                     (cons #'filter: (POLY a (make-arrow #f
                                                         (list (make-arrow #f 
                                                                           (list a) 
                                                                           B)
                                                               (make-listof #f a))
                                                         (make-listof #f a))))
                     (cons #'foldl: (POLY a
                                          (POLY b
                                                (make-arrow #f
                                                            (list (make-arrow #f (list a b) b)
                                                                  b
                                                                  (make-listof #f a))
                                                            b))))
                     (cons #'foldr: (POLY a
                                          (POLY b
                                                (make-arrow #f
                                                            (list (make-arrow #f (list a b) b)
                                                                  b
                                                                  (make-listof #f a))
                                                            b))))
                     (cons #'reverse: (POLY a (make-arrow #f
                                                          (list (make-listof #f a))
                                                          (make-listof #f a))))
                     (cons #'append: (POLY a (make-arrow #f
                                                         (list (make-listof #f a)
                                                               (make-listof #f a))
                                                         (make-listof #f a))))
                     (cons #'box: (POLY a (make-arrow #f
                                                      (list a)
                                                      (make-boxof #f a))))
                     (cons #'unbox: (POLY a (make-arrow #f
                                                        (list (make-boxof #f a))
                                                        a)))
                     (cons #'set-box!: (POLY a (make-arrow #f
                                                           (list (make-boxof #f a) a)
                                                           (make-vd #f))))
                     (cons #'make-vector (POLY a (make-arrow #f
                                                             (list N a)
                                                             (make-vectorof #f a))))
                     (cons #'vector-ref (POLY a (make-arrow #f
                                                            (list (make-vectorof #f a)
                                                                  N)
                                                            a)))
                     (cons #'vector-set!: (POLY a (make-arrow #f
                                                              (list (make-vectorof #f a)
                                                                    N
                                                                    a)
                                                              (make-vd #f))))
                     (cons #'vector-length (POLY a (make-arrow #f
                                                               (list (make-vectorof #f a))
                                                               N)))

                     (cons #'make-parameter (POLY a (make-arrow #f
                                                                (list a)
                                                                (make-parameterof #f a))))
                     (cons #'parameter-ref (POLY a (make-arrow #f
                                                               (list (make-parameterof #f a))
                                                               a)))
                     (cons #'parameter-set! (POLY a (make-arrow #f
                                                                (list (make-parameterof #f a)
                                                                      a)
                                                                (make-vd #f))))
                     

                     (cons #'string-append (make-arrow #f 
                                                       (list STR STR)
                                                       STR))
                     (cons #'string->symbol (make-arrow #f 
                                                        (list STR)
                                                        SYM))
                     (cons #'symbol->string (make-arrow #f 
                                                        (list SYM)
                                                        STR))
                     (cons #'identity: (POLY a
                                             (make-arrow #f
                                                         (list a)
                                                         a)))
                     (cons #'to-string (POLY a
                                             (make-arrow #f
                                                         (list a)
                                                         STR)))
                     (cons #'display (POLY a
                                           (make-arrow #f
                                                       (list a)
                                                       (make-vd #f))))
                     (cons #'string-ref (make-arrow #f
                                                    (list STR N)
                                                    CHAR))
                     (cons #'string-length (make-arrow #f
                                                       (list STR)
                                                       N))
                     (cons #'substring (make-arrow #f
                                                   (list STR N N)
                                                   STR))
                     (cons #'string->list (make-arrow #f
                                                      (list STR)
                                                      (make-listof #f CHAR)))
                     (cons #'list->string (make-arrow #f
                                                      (list (make-listof #f CHAR))
                                                      STR))
                     (cons #'none: (POLY a (make-arrow #f 
                                                       (list) 
                                                       (make-datatype #f #'Optionof (list a)))))
                     (cons #'some: (POLY a (make-arrow #f 
                                                       (list a) 
                                                       (make-datatype #f #'Optionof (list a)))))
                     (cons #'none?: (POLY a (make-arrow #f 
                                                        (list
                                                         (make-datatype #f #'Optionof (list a)))
                                                        B)))
                     (cons #'some?: (POLY a (make-arrow #f 
                                                        (list
                                                         (make-datatype #f #'Optionof (list a)))
                                                        B)))
                     (cons #'some-v: (POLY a (make-arrow #f 
                                                         (list
                                                          (make-datatype #f #'Optionof (list a)))
                                                         a)))
                     (cons #'pair: (POLY a 
                                         (POLY b
                                               (make-arrow #f
                                                           (list a b)
                                                           (make-tupleof #f (list a b))))))
                     (cons #'fst (POLY a 
                                       (POLY b
                                             (make-arrow #f
                                                         (list (make-tupleof #f (list a b)))
                                                         a))))
                     (cons #'snd (POLY a 
                                       (POLY b
                                             (make-arrow #f
                                                         (list (make-tupleof #f (list a b)))
                                                         b))))
                     ))]
        [init-variants (list
                        (cons #'none: (list))
                        (cons #'some: (list (let ([a (gen-tvar #f)])
                                              (make-poly #f a a)))))])
    (typecheck-defns (expand-includes tl)
                     (append import-datatypes datatypes)
                     (append import-opaques opaques)
                     (append import-aliases aliases)
                     (append import-env init-env) 
                     (append import-variants init-variants)
                     #f
                     #f ; not top
                     null
                     #f
                     (hasheq)
                     null)))

(define-for-syntax import-datatypes null)
(define-for-syntax import-opaques null)
(define-for-syntax import-aliases null)
(define-for-syntax import-variants null)
(define-for-syntax import-env null)
(define-for-syntax (add-types! dts opqs als vars env)
  (set! import-datatypes (append dts import-datatypes))
  (set! import-opaques (append opqs import-opaques))
  (set! import-aliases (append als import-aliases))
  (set! import-variants (append vars import-variants))
  (set! import-env (append env import-env)))

(define-syntax (typecheck-and-provide stx)
  (set! module-level-expansions (reverse module-level-expansions))
  (define tl-expansions module-level-expansions)
  (define stxes (cdr (syntax->list stx)))
  (let-values ([(tys e2 dts opqs als vars macros tl-types subs)
                (with-handlers ([exn:fail? (lambda (exn)
                                             (values exn #f #f #f #f #f #f null (hasheq)))])
                  (do-original-typecheck stxes))])
    (if (exn? tys)
        ;; There was an exception while type checking. To order
        ;; type-checking errors after expansion, push the error into
        ;; a sub-expression:
        #`(#%expression (let-syntax ([x (raise #,tys)])
                          x))
        (generate-provides tys e2 dts opqs als vars macros tl-types subs (and (pair? stxes)
                                                                              (car stxes))))))

(define-for-syntax (generate-provides tys e2 dts opqs als vars macros tl-types subs stx)
  ;; don't export inaccessible identifiers that were macro-introduced:
  (define (accessible? id)
    (or (not stx)
        (identifier-binding (datum->syntax stx (syntax-e id)))))
  (define accessible-tl-types
    (filter (lambda (tl-type) (accessible? (car tl-type)))
            tl-types))
  #`(begin
      ;; Put all contracts implementations in a submodule,
      ;; so they're not loaded in a typed context:
      (module* with-contracts #f
        (begin) ; work around a bug in v6.1.1 and earlier
        (provide
         (contract-out
          #,@(map (λ (tl-thing)
                    #`[#,(car tl-thing)
                       #,(to-contract (cdr tl-thing) #f)])
                  accessible-tl-types))))
      ;; Export identifiers for untyped use as redirections to the
      ;; submodule:
      (module with-contracts-reference racket/base
        (require flit/private/contract-support)
        (define-runtime-module-path-index contracts-submod
          '(submod ".." with-contracts))
        (provide contracts-submod))
      (require (for-syntax (submod "." with-contracts-reference)))
      #,(let ([names (map (lambda (_) (gensym)) accessible-tl-types)]
              [tl-names (map car accessible-tl-types)])
          #`(begin
              (define-syntaxes #,names
                ((make-make-redirects-to-contracts contracts-submod)
                 (syntax->list (quote-syntax #,tl-names))))
              (provide #,@(for/list ([name (in-list names)]
                                     [tl-name (in-list tl-names)])
                            #`(rename-out [#,name #,tl-name])))))
      ;; Also, export type definitions:
      (provide #,@(filter accessible? (map car dts)))
      ;; And aliases
      (provide #,@(filter accessible? (map car als)))
      ;; Providing each binding renamed to a generated symbol doesn't
      ;; make the binding directly inaccessible, but it makes the binding
      ;; marked as "exported" for the purposes of inspector-guarded
      ;; access. (In other words, we're not trying to be as secure
      ;; as Typed Racket, snce we can rely on Racket's safety.)
      (provide
       (rename-out
        #,@(map (λ (tl-thing)
                  #`[#,(car tl-thing)
                     #,(gensym)])
                accessible-tl-types)))
      (module* #,(if untyped? #'untyped-flit #'flit) #f
        (begin-for-syntax
          (add-types!
           ;; datatypes:
           (list #,@(map (lambda (dt)
                           #`(cons (quote-syntax #,(car dt))
                                   (quote #,(cdr dt))))
                         dts))
           ;; opaques:
           (list #,@(map (lambda (dt)
                           #`(cons (quote-syntax #,(car dt))
                                   (quote-syntax #,(cdr dt))))
                         opqs))
           ;; aliases:
           (list #,@(map (lambda (a)
                           #`(list (quote-syntax #,(car a))
                                   (list #,@(map (lambda (a)
                                                   #`(quote-syntax #,a))
                                                 (cadr a)))
                                   (quote-syntax #,(caddr a))))
                         als))
           ;; variants:
           (list #,@(map (lambda (var)
                           #`(list (quote-syntax #,(car var))
                                   #,@(map (lambda (t)
                                             (to-expression t #hasheq()))
                                           (cdr var))))
                         vars))
           ;; types
           (list #,@(map (λ (tl-thing)
                           #`(cons (quote-syntax #,(car tl-thing))
                                   #,(to-expression (cdr tl-thing) #hasheq())))
                         tl-types))))
        (provide #,@(map car accessible-tl-types)
                 #,@(filter accessible? (map car dts))
                 #,@(filter accessible? (map car als))
                 #,@(filter accessible? (map car opqs))
                 #,@(filter accessible? macros)
                 ;; datatype predicates for contracts:
                 #,@(filter accessible?
                            (map (lambda (dt)
                                   (datum->syntax (car dt)
                                                  (string->symbol (format "~a?" (syntax-e (car dt))))))
                                 dts))))
      ;; Add provides to submodules, too:
      #,@(for/list ([(name vec) (in-hash subs)])
           (let-values ([(datatypes dt-len opaques o-len aliases a-len
                                    variants v-len env e-len
                                    macros tys tl-types submods)
                         (vector->values vec)])
             (define (drop l n) (reverse (list-tail (reverse l) n)))
             #`(module+ #,name
                 #,(generate-provides tys
                                      (drop env e-len)
                                      (drop datatypes dt-len)
                                      (drop opaques o-len)
                                      (drop aliases a-len)
                                      (drop variants v-len)
                                      macros
                                      (let-based-poly! tl-types fuel)
                                      submods
                                      #f))))))

(define-for-syntax ((make-make-redirects-to-contracts submod-modidx) ids)
  (define redirects
    (for/list ([id (in-list ids)])
      (define (redirect stx)
        (cond
          [(identifier? stx)
           (with-syntax ([mp (collapse-module-path-index/relative
                              submod-modidx)]
                         [id (datum->syntax id (syntax-e id) stx stx)])
             #`(let ()
                 (local-require (only-in mp [#,(datum->syntax #'mp (syntax-e #'id)) id]))
                 id))]
          [else
           (datum->syntax stx
                          (cons (redirect (car (syntax-e stx)))
                                (cdr (syntax-e stx)))
                          stx
                          stx)]))
      (define id-val (syntax-local-value id (lambda () #f)))
      (cond
        [(constructor-syntax? id-val)
         (reprovided-constructor-syntax (constructor-syntax-id id-val) redirect)]
        [else
         redirect])))
  (apply values redirects))

(define-for-syntax orig-body #f)
(define-for-syntax orig-is-lazy? #f)
(define-for-syntax orig-is-untyped? #f)
(define-for-syntax orig-fuel 100)
(define-for-syntax orig-module-level-expansions null)
(define-for-syntax (set-orig-body! v is-lazy? is-untyped? fuel expansions)
  (set! orig-body v)
  (set! orig-is-lazy? is-lazy?)
  (set! orig-is-untyped? is-untyped?)
  (set! orig-fuel fuel)
  (set! orig-module-level-expansions (map syntax-e (syntax->list expansions))))

(define-syntax (typecheck stx)
  (syntax-case stx ()
    [(_ is-lazy? is-untyped? fuel . body)
     #`(begin
         (begin-for-syntax (set-orig-body! (quote-syntax body) is-lazy? is-untyped? fuel
                                           (quote-syntax #,module-level-expansions)))
         ;; Typechecking happens after everything else is expanded:
         (typecheck-and-provide . body))]))

;; ----------------------------------------

(define-syntax (top-interaction stx)
  ;;(if (not module-loaded?) (error "Syntax and/or type errors in module, REPL disabled.")
  (if #f #f
      (begin
        (set! lazy? orig-is-lazy?)
        (set! untyped? orig-is-untyped?)
        (set! fuel orig-fuel)
        (syntax-case stx ()
          [(_ . body)
           untyped?
           #'body]
          [(_ . body)
           (let ([expanded-body (syntax-case #'body (define-type:)
                                  [(define-type: . _)
                                   ;; Can't `local-expand' without also evaluating
                                   ;; due to introduced identifiers interleaved
                                   ;; in definitions; the only point of local expansion 
                                   ;; is to check syntax, so just call the transformer
                                   ;; directly:
                                   (begin
                                     (expand-define-type #'body)
                                     #'body)]
                                  [_
                                   (local-expand #'(drop-type-decl body) 'top-level null)])])
             (unless tl-env
               (set! module-level-expansions (reverse orig-module-level-expansions))
               (let-values ([(ts e d o a vars macros tl-types subs) 
                             (do-original-typecheck (syntax->list (if orig-body
                                                                      (syntax-local-introduce orig-body)
                                                                      #'())))])
                 (set! tl-datatypes d)
                 (set! tl-opaques o)
                 (set! tl-aliases a)
                 (set! tl-env e)
                 (set! tl-variants vars)
                 (set! tl-submods subs)))
             (set! module-level-expansions #f)
             (let-values ([(tys e2 d2 o2 a2 vars macros tl-types subs) 
                           (typecheck-defns (expand-includes (list #'body))
                                            tl-datatypes tl-opaques tl-aliases tl-env tl-variants (identifier? #'body) #t
                                            null #f tl-submods null)])
               (set! tl-datatypes d2)
               (set! tl-opaques o2)
               (set! tl-aliases a2)
               (set! tl-env e2)
               (set! tl-variants vars)
               (set! tl-submods subs)
               (with-syntax ([ty ((type->datum (make-hasheq)) (car tys))]
                             [body (if lazy?
                                       #`(!! #,expanded-body)
                                       expanded-body)])
                 (if (void? (car tys))
                     #'body
                     #'(begin
                         (print-type 'ty)
                         body)))))]))))

(define (print-type t)
  (parameterize ([pretty-print-print-line
                  (lambda (line p len maxcol) 
                    (if (equal? line 0)
                        (display "- " p)
                        (if line
                            (display "\n  " p)
                            (display "\n" p)))
                    0)])
    (pretty-write t)))

;; ----------------------------------------

(define-syntax is-submodule #f)

(begin-for-syntax
  (define is-submodule? #f)
  
  (define (filter-keywords! forms)
    (syntax-case forms (is-submodule)
      [((is-submodule) . forms)
       (begin
         (set! is-submodule? #t)
         (filter-keywords! #'forms))]
      [(#:untyped . forms)
       (begin
         (set! untyped? #t)
         (filter-keywords! #'forms))]
      [(#:lazy . forms)
       (begin
         (set! lazy? #t)
         (filter-keywords! #'forms))]
      [(#:fuel amt . forms)
       (begin
         (unless (exact-nonnegative-integer? (syntax-e #'amt))
           (raise-syntax-error 'fuel "amount must be an exact nonnegative integer"
                               #'amt))
         (set! fuel (syntax-e #'amt))
         (filter-keywords! #'forms))]
      [_ forms])))


(define-for-syntax module-loaded? #f)

(define-syntax (module-begin stx)
  ;; (displayln (format "Module begin ~s" (syntax->datum stx)))
  (unless (eq? 'module-begin (syntax-local-context))
    (raise-syntax-error
     #f
     "allowed only around a module body"
     stx))
  (syntax-case stx ()
    [(_ form ...)
     (let ([ret
            (with-syntax ([(form ...) (filter-keywords! #'(form ...))])
              (with-syntax ([end (cond
                                   [is-submodule?
                                    #'(begin)]
                                   [untyped?
                                    #`(begin
                                        (typecheck #,lazy? #t #,fuel) ; sets untyped mode and laziness
                                        (provide #,(datum->syntax stx `(,#'all-defined-out))))]
                                   [else
                                    ;; (displayln (format "Begin module form ~s" #`(form ...)))
                                    #`(typecheck #,lazy? #,untyped? #,fuel form ...)])])
                #`(printing-module-begin
                   (drop-type-decl form) ...
                   end)))])
       (begin (set! module-loaded? #t)
              ret))]))

(define-syntax drop-type-decl
  (syntax-rules (:)
    [(_ (_ : . _)) (void)]
    [(_ other) other]))

(define-syntax printing-module-begin
  (make-wrapping-module-begin #'print-result))

(define-syntax-rule (print-result e)
  (call-with-values (lambda () (!! e)) print-values))

(define (print-values . vs)
  (for-each (current-print) vs))

;; ----------------------------------------

(module reader syntax/module-reader
  flit)
