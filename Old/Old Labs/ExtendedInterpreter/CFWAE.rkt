#lang plai

;; internal representation of a binding
(define-type Binding
  [binding (name symbol?) (named-expr CFWAE?)])

;; internal representation of the CFWAE grammar
(define-type CFWAE
  [num (n number?)]
  [binop (op procedure?) (lhs CFWAE?) (rhs CFWAE?)]
  [with (lob (listof Binding?)) (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?) (t CFWAE?) (e CFWAE?)]
  [fun (args (listof symbol?)) (body CFWAE?)]
  [app (f CFWAE?) (args (listof CFWAE?))])

;; internal representation of possible return values
(define-type CFWAE-Value
  [numV (n number?)]
  [closureV (params (listof symbol?))
            (body CFWAE?)
            (env Env?)])

;; internal representation of an environment
(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFWAE-Value?) (env Env?)])

;; internal representation of each binop and its procedure
(define binops
  (list (cons '+ (lambda (lhs rhs) (do-binop + lhs rhs)))
        (cons '- (lambda (lhs rhs) (do-binop - lhs rhs)))
        (cons '* (lambda (lhs rhs) (do-binop * lhs rhs)))
        (cons '/ (lambda (lhs rhs) (do-binop
                                (lambda (lhs rhs)
                                  (if (= 0 rhs);if
                                      (error "Divide by zero error!");then
                                      (/ lhs rhs)));else
                                lhs rhs)))))

;; set of reserved words 
(define key-words
  (set-union (set 'if0 'with 'fun '+ '- '* '/)))

;; do-binop : (operator? op numV? lhs numV? rhs) -> numV
;; Performs the arithmetic expression and returns the resulting
;; value, or else reports an error if given anything other than numbers
(define (do-binop op lhs rhs)
  (cond
    [(and (numV? lhs) (numV? rhs))
     (numV (op (numV-n lhs) (numV-n rhs)))]
    [else (error "Only numbers allowed")]))

;; lookup : symbol Env -> CFWAE-Value
;; looks up an identifier in an environment and returns the value
;; bound to it (or reports error if not found)
(define (lookup name env)
  (type-case Env env
    [mtEnv () (error 'lookup "no binding for identifier")]
    [anEnv (bound-name bound-value rest-env)
           (if (symbol=? bound-name name)
               bound-value
               (lookup name rest-env))]))

;; --------------------------------------------------------



;; --------------------------------------------------------

;; num+ : numV numV −> numV
;; adds two numV number representations
(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))

;; num- : numV numV -> numV
;; subtracts two numV number representations
(define (num- n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))

;; num* : numV numV -> numV
;; multiplies two numV number representations
(define (num* n1 n2)
  (numV (* (numV-n n1) (numV-n n2))))

;; num/ : numV numV -> numV
;; divides two numV number representations
(define (num/ n1 n2)
  (numV (/ (numV-n n1) (numV-n n2))))

;; parse : sexp −> CFWAE
;; to convert s-expressions into CFWAEs
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (if (not (set-member? key-words sexp));if sexp is not a member of key-words
                        (id sexp); then we're good
                        (error "Reserved names cannot be used as IDs"))];else error
    [(list? sexp)
     (case (first sexp)
       [(operator?) (if (= 3 (length sexp))                                  ;if the length is 3
                        (binop (dict-ref binops (first sexp))                ;then parse
                               (parse (second sexp))
                               (parse (third sexp)))
                        (error "Binary operation should only take 2 arguments"))] ;else error
       [(if0) (if (= 4 (length sexp))                                        ;if the length is 4
                  (if0 (parse (second sexp))                                 ;the parse it
                       (parse (third sexp))
                       (parse (fourth sexp)))
                  (error "if0 should have a Test for 0, a Then branch, and an Else branch"))];else error
       [(with) (if (= 3 (length sexp))
                   (with (parse-bindings (second sexp))
                         (parse (third sexp)))
                   (error "with expression requires a list of bindings and a body"))]
       [(fun) (if (= 3 (length sexp))
                  (fun (parse-args (second sexp))
                       (parse (third sexp)))
                  (error "fun expression should have an argument list and a body"))]
       [(app) (app (parse (first sexp))
                   (map parse (rest sexp)))]
       [else (error "Parse error!")])]))

;; Part of parse
(define (parse-bindings sexp)
  (cond
    [(not (and (list? sexp)
               (andmap (lambda (l) (and (list? l)
                                        (= 2 (length l))
                                        (symbol? (first l))))
                       sexp)))
     (error "Bindings should be a list of pairs")]
    [(not (= (length sexp)
             (set-count (list->set (map first sexp)))))
     (error "Identifiers should be unique")]
    [else (map (lambda (l) (binding (first l)
                                    (parse (second l))))
               sexp)]))

;; Part of parse
(define (parse-args sexp)
  (if (and
       (list? sexp)
       (andmap symbol? sexp)
       (= (length sexp)
          (set-count (list->set sexp))))
      sexp
      (error "arguments should be unique symbols")))




;; interp : CFWAE Env -> CFWAE-Value
;; evaluates an expression with respect to the current environment
(define (interp expr env)
  (type-case CFWAE expr
    [num (n) (numV n)]
    [id (v) (lookup v env)]
    [binop (op l r) (op (interp l env)
                        (interp r env))]
;    [binop (op l r)
;           (case op
;             [(+) (num+ (interp l env) (interp r env))]
;             [(-) (num- (interp l env) (interp r env))]
;             [(*) (num* (interp l env) (interp r env))]
;             [(/) (num/ (interp l env) (interp r env))])]
    [if0 (c t e) (local [(define cv (interp c env))]
                   (cond
                     [(not (numV? cv)) (error "Test must be a number")]
                     [else (if (zero? cv);(equal? (numV 0) cv)
                               (interp t env)
                               (interp e env))]))]
    [with (lob body) (local [(define new-env
                               (foldl (lambda (b e)
                                        (anEnv (binding-name b)
                                               (interp (binding-named-expr b) env)
                                               e))
                                      env lob))]
                       (interp body new-env))]
;    [with (bound-id named-expr bound-body)
;          (interp bound-body
;                  (anEnv bound-id
;                         (interp named-expr
;                                 env)
;                         env))]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
;    [app (fun-expr args-expr) (local [(define c (interp-in-env f env))]
;                    (if (and (closureV? c)
;                             (= (length (closureV-params c))
;                                (length args)))
;                        (interp-in-env (closureV-body c)
;                                       (foldl (lambda (a ex e) (anEnv a (interp ex env) e))
;                                              (closureV-env c)
;                                              (closureV-params c)
;                                              args))
;                        (error "Only functions can be applied")))]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr env)])
           (interp (closureV-body fun-val)
                   (anEnv (closureV-params fun-val)
                          (interp arg-expr env)
                          (closureV-env fun-val))))]
    )
  )




;; run : s-expression -> numV
;; parses then evaluates an s-expression in the FWAE language
(define (run expr) 
  (interp 
   (parse expr)
   (mtEnv)))



;; -- TESTS --

;Literals:
(test (parse 3) (num 3))
(test (parse 'x) (id 'x))

;Binop:
(test (parse (+ 3 4)) (num 7))
(test (parse (- 3 4)) (num -1))
(test (parse (* 3 4)) (num 12))
(test (parse (/ 10 2)) (num 5))
(test/exn (run '{{fun {x} {* x}} 2}) "2 arguments")
(test/exn (run '{{fun {x y z} {* x y z}} 2 3 4}) "2 arguments")

;ID:
(test/exn (parse '+) "cannot be used")
(test/exn (parse '-) "cannot be used")
(test/exn (parse '*) "cannot be used")
(test/exn (parse '/) "cannot be used")
(test/exn (parse 'with) "cannot be used")
(test/exn (parse 'if0) "cannot be used")
(test/exn (parse 'fun) "cannot be used")

;if0:
(test (parse (if0 (num 0) (id 'true) (id 'false))) (id 'true))
(test/exn (parse (if0 (num 0) (id 'false))) "does not match")
(test/exn (parse (if0 (num 0) (id 'true) (id 'false) (id 'extra))) "does not match")



(run '{fun {x} x})


