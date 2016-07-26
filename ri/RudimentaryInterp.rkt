#lang plai

;; type Binding
(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

;; type WAE
(define-type WAE
  [num (n number?)]
  [binop (op procedure?)
         (lhs WAE?)
         (rhs WAE?)]
  [with (lob (listof Binding?)) (body WAE?)]
  [id (name symbol?)])



;; op-table : (listof (list/c symbol? (number? number? . -> . number?)))
;; A data-structure to contain a mapping from operator symbols to their definitions
(define op-table
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)))



;; lookup-op : symbol -> (or/c procedure false)
;; Extracts the definition of an operator or false
(define (lookup-op op)
  (if
    (assoc op op-table)
    (second (assoc op op-table))
    false))


(test (lookup-op '+) +)
(test (lookup-op '-) -)
(test (lookup-op '*) *)
(test (lookup-op '/) /)
(test (lookup-op '$) false)





;; parse : sexp −> WAE
;; to convert s-expressions into WAEs
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
     (case (first sexp)
       [(with)
        (if (list? (second sexp))
            (with
              (map parse-binding (second  sexp))
              (parse (third sexp)))
            (error "Not a list of bindings"))]
       [(id) (sexp)]
       [else (cond
                [(lookup-op (first sexp))   ;; calls lookup-op
                 (binop (lookup-op (first sexp)) 
                        (parse (second sexp))
                        (parse (third sexp)))]
                [else (error "Invalid symbol")])])]
    [else (error "Illegal syntax")]))



;; parse-binding
(define (parse-binding l)
  (if (list? l)
      (if (= 2 (length l))
          (binding (first l) (parse (second l)))
          (error "Too few args in binding"))
      (error "Not a list of bindings")))


(test (parse '5) (num 5))
(test (parse '+) (id '+))
(test (parse '-) (id '-))
(test (parse '*) (id '*))
(test (parse '/) (id '/))




;; subst* : (listof Binding) WAE -> WAE
;; Substitutes for all of the bindings in lob inside body simultaneously
(define (subst* lob body)
  (foldr (λ (binding expr) 
           (subst 
            expr 
            (binding-name binding) 
            (binding-named-expr binding)))
         body lob))




; subst : WAE symbol number -> WAE
(define (subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [binop (op l r) (binop op 
                      (subst l sub-id val)
                      (subst r sub-id val))]
    [with (bound bound-body)
          
          (if (symbol=? (binding-name bound) sub-id)
              (with (binding (binding-name bound)
                             (subst (binding-named-expr bound) sub-id val))
                    bound-body) 
              (with (binding (binding-name bound)
                             (subst (binding-name bound) sub-id val))
                    (subst bound-body sub-id  val)))]))




;; calc : WAE -> number
;; evaluates WAE expressions by reducing them to numbers
(define (calc expr)
  (type-case WAE expr
    [num (n) n]
    [binop (op l r) (op (calc l) (calc r))]
    ; new from here
    [with (bind body)
          (calc (subst body
                       (binding-name bind)
                       (num (calc 
                             (binding-named-expr bind)))))]
    [id (v) (error 'calc "free identifier")]))







