#lang plai

;(print-only-errors)

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



;;procedure
;;(parse sexp) → Expr?
;;  sexp : s-expression?

;returns true or false based on if sexp is a valid expression or not.
; parse : s-expression -> Expr

(define bad-list (list 'false 'true #t #f 't-bool 
                       't-nlist 't-num 't-fun 'app 
                       'with 'nempty 'iszero 'bif
                       'bool 'num 'id 'ncons 'nfirst
                       'nrest 'fun 'boolean
                       'bin-num-op 'isnempty 'nempty?
                       ))

(define op-table
  (list (list '+ +)
        (list '- -)
        (list '* *)
        )
  )


;CONTRACT:(lookup-op op) → (or/c procedure? false/c)
;  op : symbol?

;PURPOSE:extracts the definition 
;of an operator or false

;DEFINITION
(define (lookup-op op)
  (if(assoc op op-table)
     (second (assoc op op-table))
     false)
  )


(define (parse-type val)
  (cond
    [(equal? 'number val) (t-num)]
    [(equal? 'boolean val) (t-bool)]
    [(equal? 'nlist val) (t-nlist)]
    [(and 
      (list? val) 
      (equal? (length val) 3)
      (equal? (second val) '->))
     
     (parse-type (first val))
     (parse-type (third val))]
    [else
     (error "Invalid token")]
    )
  )



(define (parse sexp)
  (cond
    
    ;number
    [(number? sexp) (num sexp)]
    
    ;id
    [(and (symbol? sexp)
          (not (member sexp bad-list)))
     
     (id sexp)]
    
    ;boolean
    [(equal? sexp 'true)
     (bool #t)]
    [(equal? sexp 'false)
     (bool #f)] 
    
    ;nempty
    [(equal? sexp 'nempty)
     
     (nempty)]
    
    [(list? sexp)
     (cond 
       
       ;iszero
       [(and (equal? (length sexp) 2)
             (equal? (first sexp) 'iszero))
        
        (iszero (parse (second sexp)))]
       
       ;with
       [(and (equal? (first sexp) 'with)
             (equal? (length sexp) 3)
             (list? (second sexp))
             (symbol? (first(second sexp)))
             (not (member (first (second sexp)) bad-list))
             (equal? (length (second sexp)) 2))
        
        
        (with (first (second sexp))
              (parse (second (second sexp)))
              (parse (third sexp)))]
       ;;bif
       [(and (equal? (first sexp) 'bif)
             (equal? (length sexp) 4))
        
        (bif 
         (parse (second sexp))
         (parse (third sexp))
         (parse (fourth sexp)))]
       
       ;fun
       [(and (equal? (length sexp) 5)
             (list? (second sexp))
             (equal? (length (second sexp)) 3)
             (symbol?(first (second sexp)))
             (not (member (first (second sexp)) bad-list))
             (equal? (first sexp) 'fun)
             (equal? (second (second sexp)) ':)
             (equal? (third sexp) ':))
        
        (fun
         (first (second sexp))
         (parse-type (third (second sexp)))
         (parse-type (fourth sexp)) 
         (parse (fifth sexp)))]
       
       ;ncons
       [(and (equal? (first sexp) 'ncons)
             (equal? (length sexp) 3))
        
        (ncons (parse (second sexp))
               (parse (third sexp)))]
       
       ;nempty
       [(and (equal? (first sexp) 'nempty?)
             (equal? (length sexp) 2))
        
        (isnempty (parse (second sexp)))]
       
       ;nfirst
       [(and (equal? (first sexp) 'nfirst)
             (equal? (length sexp) 2))
        
        (nfirst (parse (second sexp)))]
       
       ;nrest
       [(and (equal? (first sexp) 'nrest)
             (equal? (length sexp) 2))
        
        (nrest (parse (second sexp)))]
       
       [(id? (first sexp)) (id)]
       
       ;app
       [(equal? (length sexp) 2)
        
        (app (parse (first sexp))
             (parse (second sexp)))]
       [else (cond
               [(lookup-op (first sexp)) 
                (bin-num-op (lookup-op (first sexp)) 
                            (parse (second sexp))
                            (parse (third sexp)))]
               [else (error "unrecognized symbol")]
               )
             ]
       )
     ]
    [else (error "Illegal syntax")]
    )
  )

(define-type Type-Env
  [mtenv]
  [nenv (bound-id symbol?)
        (bound-type Type?)
        (type-env Type-Env?)])


;;(type-of e) → Type?
;;e : Expr?


(define (type-of sexp)
  (type-of* sexp (mtenv))
  )

(define (look-up-type val env)
  (type-case Type-Env env
    [mtenv () (error "No matching value in env")]
    [nenv (n j k) (if (equal? val n)
                      j
                      (look-up-type val k))]
    )
  )

;(type-of (parse sexp)) 
; returns  a valid type
; type-of : Expr -> Type
(define (type-of* e env)
  (type-case Expr e
    
    [num (n) (t-num)];check
    
    [id  (n) (look-up-type n env)];check
    
    [bool (n) (t-bool)];check
    
    [bin-num-op (proc l r) 
                (if (and (equal? (t-num) (type-of* l env))
                         (equal? (t-num) (type-of* r env)))
                    (t-num)
                    (error "Cannot apply binop to invalid operands"))]
    [iszero  (n) (if (equal? (type-of* n env) (t-num))
                     (t-bool)
                     (error "iszero expects a number"))]
    
    [bif  (test then else) (if (equal? (type-of* test env) (t-bool))
                               (if (equal? (type-of* then env)
                                           (type-of* else env))
                                   (type-of* then env)
                                   (error "unmatching type of branches"))
                               (error "bif condition is not a t-bool"))]
    
    [with  (id val body) (type-of* body (nenv id (type-of* val env) env))]
    
    [fun  (id ptype ftype body) (if (equal? 
                                     (type-of* body (nenv id ptype env)) 
                                     ftype)
                                    (t-fun ptype ftype)
                     (error "fun body does not evaluate to given type"))]
    [app  (n j) (type-case Type (type-of* n env)
                  [t-fun (in out) 
                         (if(equal? (type-of* j env) in)
                            (type-of* out env)
                            (error "not a function"))]
                  [else (error "not a function")])]
    [nempty () (t-nlist)]
    [ncons  (n l) (if (equal? (type-of* n env) (type-of* l env))
                      (t-nlist)
                      (error "not a real nlist"))]
    [nfirst (n) (type-of* n env)]
    [nrest  (n) (t-nlist)]
    [isnempty (n) (t-bool)]
    )
  
  )
;  [num (l r) (and (equal? (type-of l) 't-num)
;                 (equal? (type-of r) )]


;(type-of (parse '{+ 1 2}))
;(t-num)

;(type-of (parse '{3 4}))
;(error 'type-of "Number is not a function")

;;dont worry about:
;(nfirst nempty)

;;NUMS---------------
(test (type-of (parse 5)) (t-num))
(test (type-of (parse 0)) (t-num))
(test (type-of (parse -1)) (t-num))




;;BOOLS---------------
(test      (type-of (parse 'true)) (t-bool))
(test      (type-of (parse 'false)) (t-bool))
(test/exn  (type-of (parse #t)) "Illegal syntax")

;;BINOPS----------------------
(test (type-of (parse '(+ 3 4))) (t-num))
(test (type-of (parse '(- 3 4))) (t-num))
(test (type-of (parse '(* 3 4))) (t-num))
(test (type-of (parse '(+ 3 ( + 5 6)))) (t-num))
(test/exn (type-of (parse '(+ true ( + 5 6)))) "Cannot apply binop to invalid operands")
(test/exn (type-of (parse '(+ fun ( + 5 6)))) "Illegal syntax")
(test/exn (type-of (parse '(+ x x))) "No matching value in env")
(test/exn (type-of (parse '(+ 0 x))) "No matching value in env")

;;ISZERO-----------------------
(test (type-of (parse '(iszero 4))) (t-bool))
(test (type-of (parse '(iszero 0))) (t-bool))
(test (type-of (parse '(iszero (+ 3 4)))) (t-bool))
(test/exn (type-of (parse '(iszero true))) "iszero expects a number")
(test/exn (type-of (parse '(iszero x))) "No matching value in env")

;;BIF--------------------------
(test (type-of (parse '(bif true (+ 3 4) (- 3 4)))) (t-num))
(test (type-of (parse '(bif true true false))) (t-bool))
(test/exn (type-of (parse '(bif 3 (+ 3 4) (- 3 4)))) "bif condition is not a t-bool")
(test/exn (type-of (parse '(bif true true (- 3 4)))) "unmatching type of branches")

;;WITH-------------------------
(test (type-of (parse '(with (x 4) (+ x x)))) (t-num))
(test (type-of (parse '(with (x true) x))) (t-bool))
(test (type-of (parse '(with (x (bif true (+ 3 4) (- 3 4)) ) (+ x x)))) (t-num))
(test (type-of (parse '(with (x (with (x 6) (+ x x))) (+ x x)))) (t-num))

(test/exn (type-of (parse '(with (x true) (+ x x)))) "Cannot apply binop to invalid operands")


;;FUN-------------------------
(test (type-of (parse '(fun (y : number) :  number (+ y y)))) (t-fun (t-num) (t-num)))
;(test (type-of (parse '(with (x (fun (y : number) :  number (+ y y))) (x 4)))) (t-num))

;;APP-------------------------
;(parse '((fun (y : number) :  number (+ y y)) 4))

;;NCONS------------------------
(test (type-of (parse '(ncons 3 4))) (t-nlist))
(test/exn (type-of (parse '(ncons 3 true))) "not a real nlist")

;;NEMPTY--------------------
(test (type-of (parse 'nempty)) (t-nlist))

;;NFIRST----------------------
(test (type-of (parse '(nfirst (+ 3 4)))) (t-num))