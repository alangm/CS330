#lang plai

; abstract syntax -- no change from FWAE
(define-type CFAE/L
  [num (n number?)]
  [add (lhs CFAE/L?) (rhs CFAE/L?)]
  [with (name symbol?) (named-expr CFAE/L?) (body CFAE/L?)]
  [id (name symbol?)]
  [fun (param symbol?) (body CFAE/L?)]
  [app (fun-expr CFAE/L?) (arg-expr CFAE/L?)])

; internal representation of values
(define-type CFAE/L-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body CFAE/L?)
            (env Env?)]
  [exprV (expr CFAE/L?) ; a thunk or promise
         (env Env?)])

;; internal representation of an environment
(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFAE/L-Value?) (env Env?)])

;; parse : sexp −> CFAE/L
;; to convert s-expressions into CFAE/Ls
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(with) (with (first (second sexp))
                     (parse (second (second sexp)))
                     (parse (third sexp)))]
       [(fun) (fun (first (second sexp))
                   (parse (third sexp)))]
       [else (app (parse (first sexp))
                  (parse (second sexp)))])]))

;; interp : CFAE/L Env -> CFAE/L-Value
(define (interp expr env)
  (type-case CFAE/L expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [id (v) (strict (lookup v env))]
    [with (bound-id named-expr bound-body)
          (interp bound-body
                  (anEnv bound-id
                         (exprV named-expr env) ; make a thunk instead of evaluating
                         env))]             
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (strict (interp fun-expr env))] ; strictness using a function
                 [define arg-val (exprV arg-expr env)])  ; make a thunk instead of evaluating
           (interp (closureV-body fun-val)
                   (anEnv (closureV-param fun-val)
                          arg-val
                          (closureV-env fun-val))))]))

;; lookup : symbol Env -> CFAE/L-Value
(define (lookup name env)
  (type-case Env env
    [mtEnv () (error 'lookup "no binding for identifier")]
    [anEnv (bound-name bound-value rest-env)
           (if (symbol=? bound-name name)
               bound-value
               (lookup name rest-env))]))

;; num+ : CFAE/L-Value CFAE/L-Value −> numV
(define (num+ n1 n2)
  (numV (+ (numV-n (strict n1)) (numV-n (strict n2))))) ; strictness points before doing math

;; num-zero? : CFAE/L-Value -> boolean
(define (num-zero? n)
  (zero? (numV-n (strict n)))) ; strictness point before testing a value

;; strict : CFAE/L-Value -> CFAE/L-Value [excluding exprV]
;; forces evaluation of a thunk until you get an actual value
(define (strict e)
  (type-case CFAE/L-Value e
    [exprV (expr env)
           (local ([define the-value (strict (interp expr env))])
             (begin
               ; uncomment the following line to see when evaluation is forced
               ; (printf "Forcing exprV to ~a ~n" the-value)
               the-value))]
    [else e]))

;; run s-expression -> CFAE/L-Value
(define (run expr) 
  (strict ; strictness point before returning value to the user
   (interp 
    (parse expr)
    (mtEnv))))

; Examples

(run '{{fun {x} 2} z})

(run '{with {x z} 2})

(run '{with {x {+ 4 5}}
            {with {y {+ x x}}
                  {with {z y}
                        {with {x 4}
                              z}}}})

;;(run '{with {y blah} y})

(interp (parse '{with {y blah} y}) (mtEnv))
