#lang plai


;; Grammar:
;;
;; CFWAE    =   number
;;    |   (+ CFWAE CFWAE)
;;    |   (- CFWAE CFWAE)
;;    |   (* CFWAE CFWAE)
;;    |   (/ CFWAE CFWAE)
;;    |   id
;;    |   (if0 CFWAE CFWAE CFWAE)
;;    |   (with ([id CFWAE] ...) CFWAE)
;;    |   (fun (id ...) CFWAE)
;;    |   (CFWAE CFWAE ...)
;;

(define-type Binding
  [binding (name symbol?) (named-expr CFWAE?)])

(define-type CFWAE
  [num (n number?)]
  [binop (op procedure?) (lhs CFWAE?) (rhs CFWAE?)]
  [with (lob (listof Binding?)) (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?) (t CFWAE?) (e CFWAE?)]
  [fun (args (listof symbol?)) (body CFWAE?)]
  [app (f CFWAE?) (args (listof CFWAE?))])
 
(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFWAE-Value?) (env Env?)])
 
(define-type CFWAE-Value
  [numV (n number?)]
  [closureV (params (listof symbol?))
            (body CFWAE?)
            (env Env?)])



;; lookup : symbol Env -> CFWAE-Value
;; looks up an identifier in an environment and returns the value
;; bound to it (or reports error if not found)
(define (lookup name env)
  (type-case Env env
    [mtEnv () (error "no binding for identifier")]
    [anEnv (bound-name bound-value rest-env)
           (if (symbol=? bound-name name)
               bound-value
               (lookup name rest-env))]))




;; num+ : numV numV −> numV
;; adds two numV number representations
(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))

(define (num-binop op l r)
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else (error "both arguments should be numbers")]))

(define binops
  (list (cons '+ (lambda (l r) (num-binop + l r)))
        (cons '- (lambda (l r) (num-binop - l r)))
        (cons '* (lambda (l r) (num-binop * l r)))
        (cons '/ (lambda (l r) (num-binop
                                (lambda (l r)
                                  (if (= 0 r)
                                      (error "division by zero")
                                      (/ l r)))
                                l r)))))




; parse : expression -> CFWAE
; This procedure parses an expression into a CFWAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (parse-id sexp)]
    [(and (list? sexp) (not (empty? sexp)))
     (local [(define name (first sexp))]
       (if (set-member? reserved-symbols name)
           (cond 
             [(symbol=? name 'if0) (parse-if0 sexp)]
             [(symbol=? name 'with) (parse-with sexp)]
             [(symbol=? name 'fun) (parse-fun sexp)]
             [else (parse-binop sexp)])
           (parse-app sexp)))]
    [else (error "parse error")]))


(define reserved-symbols
  (set-union (set 'if0 'with 'fun)
             (list->set (dict-keys binops))))

(define (parse-id sexp)
  (if (not (set-member? reserved-symbols sexp))
      (id sexp)
      (error "reserved names cannot be used")))

(define (parse-if0 sexp)
  (cond
    [(= 4 (length sexp))
     (if0 (parse (second sexp))
          (parse (third sexp))
          (parse (fourth sexp)))]
    [(> 4 (length sexp)) (error "if0 has too few arguments")]
    [else (error "if0 has too many arguments")]))

(define (parse-bindings sexp)
  (cond
    [(not (list? sexp))
     (error "bindings should be a list of pairs")]
    [(not (andmap (lambda (l) (and (list? l)               ; it's a list
                                   (= 2 (length l))        ; of length 2
                                   (symbol? (first l))))   ; and the first item is a symbol
                  sexp))
     (error "binding pairs should consist of a symbol and an expression")]
    [(not (= (length sexp)
             (set-count (list->set (map first sexp)))))
     (error "identifiers in a binding list cannot be duplicates")]
    [else (map (lambda (l) (binding (first l)
                                    (parse (second l))))
               sexp)]))

(define (parse-with sexp)
  (if (= 3 (length sexp))
      (with (parse-bindings (second sexp))
            (parse (third sexp)))
      (error "with should have a binding part and a body")))

(define (parse-args sexp)
  (if (and
        (list? sexp)
        (andmap symbol? sexp)
        (= (length sexp)
           (set-count (list->set sexp))))
  sexp
  (error "arguments cannot be duplicates")))

(define (parse-fun sexp)
  (if (= 3 (length sexp))
      (fun (parse-args (second sexp))
           (parse (third sexp)))
      (error "fun should have an argument list and a body")))

(define (parse-app sexp)
  (app (parse (first sexp))
       (map parse (rest sexp))))

(define (parse-binop sexp)
  (cond
    [(= 3 (length sexp))
     (binop (dict-ref binops (first sexp))
            (parse (second sexp))
            (parse (third sexp)))]
    [(> 3 (length sexp)) (error "binop has too few arguments")]
    [else (error "binop has too many arguments")]))




; interp : CFWAE Env -> CFWAE-Value
; This procedure interprets the given CFWAE in the environment
; and produces a result in the form of a CFWAE-Value
(define (interp expr env)
  (type-case CFWAE expr
    [num (n) (numV n)]
    [id (n) (lookup n env)]
    [binop (op lhs rhs) (op (interp lhs env)
                            (interp rhs env))]
    [with (lob body) (local [(define new-env
                               (foldl (lambda (b e)
                                        (anEnv (binding-name b)
                                               (interp (binding-named-expr b) env)
                                               e))
                                      env lob))]
                       (interp body new-env))]
    [if0 (c t e) (local [(define cv (interp c env))]
                   (cond
                     [(not (numV? cv)) (error "test expression evalute to a number")]
                     [else (if (equal? (numV 0) cv)
                               (interp t env)
                               (interp e env))]))]
    [fun (args body) (closureV args body env)]
    [app (f args) (local [(define c (interp f env))]
                    (if (and (closureV? c)
                             (= (length (closureV-params c))
                                (length args)))
                        (interp (closureV-body c)
                                       (foldl (lambda (a ex e) (anEnv a (interp ex env) e))
                                              (closureV-env c)
                                              (closureV-params c)
                                              args))
                        (error "only functions can be applied")))]))



;; run : s-expression -> numV
;; parses then evaluates an s-expression in the FWAE language
(define (run expr) 
  (interp 
   (parse expr)
   (mtEnv)))

;; -- some examples --

;(run '(with {double {fun {x} {+ x x}} 5} {double 5}))

(run '{fun {x} x})

(run '{fun {x}
           {fun {y} 
                {+ x y}}})

(run '{
       {fun {x}
            {fun {y} 
                 {+ x y}}}
       3
       })





;; Parse Tests

(test (parse '3) (num 3))
(test/exn (parse '"3") "parse error")

(test (parse (+ 2 2)) (num 4))
      (parse '(+ 2 2))
(test (parse (- 5 2)) (num 3))
      (parse '(- 5 2))
(test (parse (* 2 10)) (num 20))
      (parse '(* 2 10))
(test (parse (/ 10 2)) (num 5))
      (parse '(/ 10 2))
(test/exn (parse '(+ 2)) "binop has too few arguments")
(test/exn (parse '(- 10 5 2)) "binop has too many arguments")

(test (parse 'a) (id 'a))
(test/exn (parse '+) "reserved names cannot be used")
(test/exn (parse '-) "reserved names cannot be used")
(test/exn (parse '*) "reserved names cannot be used")
(test/exn (parse '/) "reserved names cannot be used")
(test/exn (parse 'with) "reserved names cannot be used")
(test/exn (parse 'if0) "reserved names cannot be used")
(test/exn (parse 'fun) "reserved names cannot be used")

(parse '(if0 0 1 2))
(parse '(if0 1 2 3))
(test/exn (parse '(if0 1 2)) "if0 has too few arguments")
(test/exn (parse '(if0 1 2 3 4)) "if0 has too many arguments")

(parse '(with ((x 2) (y 4)) (if0 1 x y)))
(test/exn (parse '((with ((x 2) (y 4))))) "with should have a binding part and a body")
(test/exn (parse '(with ((x 2) (y 4)) (if0 1 x y) (if0 1 x y))) "with should have a binding part and a body")
(test/exn (parse '(with ((2) (y 4)) (if0 1 x y))) "binding pairs should consist of a symbol and an expression")
(test/exn (parse '(with ((x 4 2) (y 4)) (if0 1 x y))) "binding pairs should consist of a symbol and an expression")

(parse '(fun (x y) (* x y)))
(test/exn (parse '(fun (* x y))) "fun should have an argument list and a body")
(test/exn (parse '(fun (x y) (x) (* x y))) "fun should have an argument list and a body")

(parse '(app (1 2)))

(test/exn (parse '()) "parse error")

;; Interp Tests

(test (run 4) (numV 4))

(test (run '(+ 4 4)) (numV 8))
(test (run '(+ 2 20)) (numV 22))
(test (run '(- 4 4)) (numV 0))
(test (run '(- 14 4)) (numV 10))
(test (run '(* 4 4)) (numV 16))
(test (run '(* 1 4)) (numV 4))
(test (run '(/ 4 4)) (numV 1))
(test/exn (run '(/ 10 0)) "division by zero")

(test (run 'a) "no binding for identifier")

(test (run '(if0 0 1 2)) (numV 1))
(test (run '(if0 1 2 3)) (numV 3))

(test (run '((fun (x y) (* x y)) 2 3)) (numV 6))
(test (run '((fun (x y z) (* (* x y) z)) 2 3 4)) (numV 24))

(test (run '(with ((x 2) (y 4)) (if0 1 x y))) (numV 4))
(test (run '(with ((x 2) (y 4)) (if0 0 x y))) (numV 2))



