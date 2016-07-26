#lang plai

(define-type Expr
  [num (n number?)]
  [id (v symbol?)]
  [bool (b boolean?)]
  [bin-num-op (op procedure?) (lhs Expr?) (rhs Expr?)]
  [iszero (e Expr?)]
  [bif (test Expr?) (then Expr?) (else Expr?)]
  [with (bound-id symbol?) (bound-body Expr?) (body Expr?)]
  [fun (arg-id symbol?)
       (arg-type Type?) (result-type Type?)
       (body Expr?)]
  [app (fun-expr Expr?) (arg-expr Expr?)]
  [nempty]
  [ncons (first Expr?) (rest Expr?)]
  [nfirst (e Expr?)]
  [nrest (e Expr?)]
  [isnempty (e Expr?)])
 
(define-type Type
  [t-num]
  [t-bool]
  [t-nlist]
  [t-fun (arg Type?) (result Type?)])

(define op-table
  (
   list (list '+ +)
        (list '- -)
        (list '* *)
        )
  )

(define (lookup-op op)
  (cond
    [(list? (assoc op op-table)) (second (assoc op op-table))]
    [else #f]
  )
  )
 
; parse : s-expression -> Expr
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(equal? 'nempty sexp) (nempty)]
    [(symbol? sexp) (id sexp)]
    [(boolean? sexp) (bool sexp)]
    [(list? sexp) (cond
                    [(procedure? (lookup-op (first sexp)))
                     (bin-num-op (lookup-op (first sexp)) (parse (second sexp)) (parse (third sexp)))]
                    [(equal? 'iszero (first sexp))
                     (iszero (parse (second sexp)))
                     ]
                    [(equal? 'bif (first sexp))
                     (bif (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)))]
                    [(equal? 'with (first sexp))
                     (with (second sexp) (parse (third sexp)) (parse (fourth sexp)))]
                    [(equal? 'fun (first sexp))
                     (fun (parse (second sexp)) (type-of (second sexp)) (type-of (third sexp)) (parse (third sexp)))]
                    [(list? (first sexp))
                     (app (parse(first sexp)) (parse (second sexp)))]
                    [(equal? 'ncons (first sexp))
                     (ncons (parse (second sexp)) (parse (third sexp)))]
                    [(equal? 'nfirst (first sexp))
                     (nfirst (parse (second sexp)))]
                    [(equal? 'nrest (first sexp))
                     (nrest (parse (second sexp)))]
                    [(equal? 'isnempty (first sexp))
                     (isnempty (parse (second sexp)))]
                    [else (error (first sexp))]
                    )]
    )
  )






;; type environment
(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (type Type?) (env Env?)])

;; lookup-type : symbol Env -> Type?
;; returns the type bound to an identifier.
(define (lookup-type name env)
  (type-case Env env
    [mtEnv () (error 'lookup-type "Unbound identifier")]
    [anEnv (bound-name bound-type rest-env)
           (if (symbol=? bound-name name)
               bound-type
               (lookup-type name rest-env))]))



;; type-of : Expr -> Type
;; returns the type of a given expression
(define (type-of e)
  (type-of-inner e (mtEnv)))


;; type-of-inner : (Expr, Env) -> Type
;; helper method for type-of
(define (type-of-inner e env)
  (type-case Expr e
    [num (n) (t-num)]
    [id (v) (lookup-type v env)]
    [bool (b) (t-bool)]
    [bin-num-op (op lhs rhs)
      (if (and (t-num? (type-of-inner lhs env))
               (t-num? (type-of-inner rhs env)))
          (t-num)
          (error "bin-num-op must contain two numbers"))]
    [iszero (e) (if (equal? (t-num) (type-of-inner e env))
                    (t-bool)
                    (error "iszero must consume a number"))]
    [bif (test then else) (if (t-bool? (type-of-inner test env))
                                  (if (equal? (type-of-inner then env) (type-of-inner else env))
                                      (type-of-inner then env)
                                      (error "Type of then and else must be the same"))
                                  (error "Condition must be a boolean"))]
    [with (bound-id bound-body body)
      (let [(new-env (anEnv bound-id
                            (type-of-inner bound-body env)
                            env))]
        (type-of-inner body new-env))]
    [fun (arg-id arg-type result-type body)
      (if (equal? result-type (type-of-inner body (anEnv arg-id
                                                         arg-type
                                                         env)))
          (t-fun arg-type result-type)
          (error "Function body does not match result-type"))]
    [app (fun-expr arg-expr)
      (if (t-fun? (type-of-inner fun-expr env))
          (if (equal? (fun-arg-type fun-expr) (type-of-inner arg-expr env))
              (fun-result-type fun-expr)
              (error "Argument type does not match type expected by function"))
          (error "Expression is not a function"))]
    [nempty () (t-nlist)]
    [ncons (first rest) (if (equal? (t-num) (type-of-inner first env))
                            (if (equal? (t-nlist) (type-of-inner rest env))
                                (t-nlist)
                                (error "Cons rest must be a list"))
                            (error "Cons first must be a number"))]
    [nfirst (e) (if (equal? (t-nlist) (type-of-inner e env))
                    (t-num)
                    (error "Argument to first must be a list"))]
    [nrest (e) (if (equal? (t-nlist) (type-of-inner e env))
                    (t-nlist)
                    (error "Argument to rest must be a list"))]
    [isnempty (e)
      (if (equal? (t-nlist) (type-of-inner e env))
          (t-bool)
          (error "Argument to isnempty must be a list"))]))



;; TESTS

;;----num-------------
(test (type-of (parse 0)) (t-num))
(test (type-of (parse 1)) (t-num))
(test (type-of (parse 2)) (t-num))
(test (type-of (parse 10)) (t-num))
(test (type-of (parse -1)) (t-num))
(test (type-of (parse -10)) (t-num))

;;----id--------------

;;----bool-----------
(test      (type-of (parse 'true)) (t-bool))
(test      (type-of (parse 'false)) (t-bool))
(test/exn  (type-of (parse #t)) (t-bool))
(test/exn  (type-of (parse #f)) (t-bool))

;;----binop------------------
(test (type-of (parse '(+ 3 4))) (t-num))
(test (type-of (parse '(- 3 4))) (t-num))
(test (type-of (parse '(* 3 4))) (t-num))
(test (type-of (parse '(+ 3 ( + 5 6)))) (t-num))
(test/exn (type-of (parse '(+ true ( + 5 6)))) "Cannot apply binop to invalid operands")
(test/exn (type-of (parse '(+ fun ( + 5 6)))) "Illegal syntax")
(test/exn (type-of (parse '(+ x x))) "No matching value in env")
(test/exn (type-of (parse '(+ 0 x))) "No matching value in env")

;;----iszero-------------------
(test (type-of (parse '(iszero 4))) (t-bool))
(test (type-of (parse '(iszero 0))) (t-bool))
(test (type-of (parse '(iszero (+ 3 4)))) (t-bool))
(test/exn (type-of (parse '(iszero true))) "iszero expects a number")
(test/exn (type-of (parse '(iszero x))) "No matching value in env")

;;----bif----------------------
(test (type-of (parse '(bif true (+ 3 4) (- 3 4)))) (t-num))
(test (type-of (parse '(bif true true false))) (t-bool))
(test/exn (type-of (parse '(bif 3 (+ 3 4) (- 3 4)))) "bif condition is not a t-bool")
(test/exn (type-of (parse '(bif true true (- 3 4)))) "unmatching type of branches")

;;----with---------------------
(test (type-of (parse '(with (x 4) (+ x x)))) (t-num))
(test (type-of (parse '(with (x true) x))) (t-bool))
(test (type-of (parse '(with (x (bif true (+ 3 4) (- 3 4)) ) (+ x x)))) (t-num))
(test (type-of (parse '(with (x (with (x 6) (+ x x))) (+ x x)))) (t-num))

(test/exn (type-of (parse '(with (x true) (+ x x)))) "Cannot apply binop to invalid operands")

;;----fun-------------------------
(test (type-of (parse '(fun (y : number) :  number (+ y y)))) (t-fun (t-num) (t-num)))
;(test (type-of (parse '(with (x (fun (y : number) :  number (+ y y))) (x 4)))) (t-num))

;;----fun---------------------
;(parse '((fun (y : number) :  number (+ y y)) 4))

;;----ncons--------------------
(test (type-of (parse '(ncons 3 4))) (t-nlist))
(test/exn (type-of (parse '(ncons 3 true))) "not a real nlist")

;;----nempty--------------------
(test (type-of (parse 'nempty)) (t-nlist))

;;----nfirst----------------------
(test (type-of (parse '(nfirst (+ 3 4)))) (t-num))
