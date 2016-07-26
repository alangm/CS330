#lang plai

(define-type Expr
  [num (n number?)]
  [id (v symbol?)]
  [bool (b boolean?)]
  [bin-num-op (op procedure?) (lhs Expr?) (rhs Expr?)]
  [iszero (e Expr?)]
  [bif (test Expr?) (then Expr?) (else Expr?)]
  [with (bound-id symbol?) (bound-body Expr?) (body Expr?)]
  [rec-with (bound-id symbol?) (bound-body Expr?) (body Expr?)]
  [fun (arg-id symbol?) (body Expr?)]
  [app (fun-expr Expr?) (arg-expr Expr?)]
  [tempty]
  [tcons (first Expr?) (rest Expr?)]
  [tfirst (e Expr?)]
  [trest (e Expr?)]
  [istempty (e Expr?)])
 
(define-type Type
  [t-num]
  [t-bool]
  [t-list (elem Type?)]
  [t-fun (arg Type?) (result Type?)]
  [t-var (v symbol?)])
 
(define-type Constraint
  [eqc (lhs Type?) (rhs Type?)])

;; type environment
(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (new-name symbol?) (env Env?)])

(define op-table
  (
   list (list '+ +)
        (list '- -)
        (list '* *)
        )
  )

;type-look-up; string -> Type
;takes type as string, returns type
(define (type-look-up t)
  (cond
    [(equal? t 't-num) (t-num)]
    [(equal? t 't-bool) (t-bool)]
    [(equal? t 't-nlist) (t-list)]
    )
  )

;lookup-op; string -> procedure
;takes string, returns procedure if string is in op-table
(define (lookup-op op)
  (cond
    [(list? (assoc op op-table)) (second (assoc op op-table))]
    [else #f]
  )
  )


;; not-keyword? : symbol -> boolean
;; To determine if a symbol is not a keyword in our grammar
(define (not-keyword? sym)
  (not (member 
         sym 
         '(true false + - * iszero bif with rec 
           fun tempty tcons tempty? tfirst trest))))




(define (parse se)
  (cond 
    [(number? se)
     (num se)]
    [(and (symbol? se)
          (or (eq? se 'true)
              (eq? se 'false)
              (eq? se 'tempty)
              (not-keyword? se)))
     (cond [(eq? se 'true) (bool true)]
           [(eq? se 'false) (bool false)]
           [(eq? se 'tempty) (tempty)]
           [else (id se)])]
    [(and (list? se)
          (= (length se) 2)
          (or (member (first se) '(iszero tempty? tfirst trest))
              (not-keyword? (first se))))
     (cond 
       [(eq? (first se) 'iszero)
        (iszero (parse (second se)))]
       [(eq? (first se) 'tempty?)
        (istempty (parse (second se)))]
       [(eq? (first se) 'tfirst)
        (tfirst (parse (second se)))]
       [(eq? (first se) 'trest)
        (trest (parse (second se)))]
       [else (app (parse (first se)) (parse (second se)))])]
    [(and (list? se)
          (= (length se) 3)
          (symbol? (first se))
          (member (first se) '(+ - * tcons)))
     (cond
       [(eq? (first se) '+)
        (bin-num-op + (parse (second se)) (parse (third se)))]
       [(eq? (first se) '-)
        (bin-num-op - (parse (second se)) (parse (third se)))]
       [(eq? (first se) '*)
        (bin-num-op * (parse (second se)) (parse (third se)))]
       [(eq? (first se) 'tcons)
        (tcons (parse (second se)) (parse (third se)))])]
    [(and (list? se)
          (= (length se) 3)
          (symbol? (first se))
          (member (first se) '(with rec))
          (list? (second se))
          (not-keyword? (first (second se)))
          (= (length (second se)) 2))
     (cond
       [(eq? (first se) 'with)
        (with (first (second se))
              (parse (second (second se)))
              (parse (third se)))]
       [(eq? (first se) 'rec)
        (rec-with (first (second se))
                  (parse (second (second se)))
                  (parse (third se)))])]
    [(and (list? se)
          (= (length se) 3)
          (eq? (first se) 'fun)
          (list? (second se))
          (= (length (second se)) 1)
          (not-keyword? (first (second se))))
     (fun (first (second se)) (parse (third se)))]
    [(and (list? se)
          (= (length se) 4)
          (symbol? (first se))
          (eq? (first se) 'bif))
     (bif (parse (second se)) 
          (parse (third se)) 
          (parse (fourth se)))] 
    [else
     (error 'parse "invalid s-expression: ~v" se)]))


;  [num (n number?)]
;  [id (v symbol?)]
;  [bool (b boolean?)]
;  [bin-num-op (op procedure?) (lhs Expr?) (rhs Expr?)]
;  [iszero (e Expr?)]
;  [bif (test Expr?) (then Expr?) (else Expr?)]
;  [with (bound-id symbol?) (bound-body Expr?) (body Expr?)]
;  [rec-with (bound-id symbol?) (bound-body Expr?) (body Expr?)]
;  [fun (arg-id symbol?) (body Expr?)]
;  [app (fun-expr Expr?) (arg-expr Expr?)]
;  [tempty]
 ; [tcons (first Expr?) (rest Expr?)]
 ; [tfirst (e Expr?)]
 ; [trest (e Expr?)]
 ; [istempty (e Expr?)])


;; alpha-vary : expr -> expr
;; To rename all identifiers in e to be globally unique in preparation for
;;  constraint generation
(define (alpha-vary e) 
  (rec-alpha-vary (make-immutable-hasheqv) e))

;; rec-alpha-vary: hash expr -> expr
;; To rename all identifiers in expr to be unique, using the replacement in
;;  hash if one exists.
(define (rec-alpha-vary hash e)
  (cond
    [(or (num? e)
         (bool? e)
         (tempty? e))
     e]
    [(id? e) (rename-id hash e)]
    [(symbol? e) (rename-symbol hash e)]
    [(bin-num-op? e) (bin-num-op 
                       (bin-num-op-op e) 
                       (rec-alpha-vary hash (bin-num-op-lhs e))
                       (rec-alpha-vary hash (bin-num-op-rhs e)))]
    [(iszero? e) (make-iszero (rec-alpha-vary hash (iszero-e e)))]
    [(bif? e) (bif (rec-alpha-vary hash (bif-test e))
                   (rec-alpha-vary hash (bif-then e))
                   (rec-alpha-vary hash (bif-else e)))]
    [(with? e)
     (let ([new-hash (hash-set hash 
                               (with-bound-id e) 
                               (gensym (with-bound-id e)))])
       (with (rec-alpha-vary new-hash (with-bound-id e))
             (rec-alpha-vary hash (with-bound-body e))
             (rec-alpha-vary new-hash (with-body e))))]
    [(rec-with? e)
     (let ([new-hash (hash-set hash 
                               (rec-with-bound-id e) 
                               (gensym (rec-with-bound-id e)))])
       (rec-with (rec-alpha-vary new-hash (rec-with-bound-id e))
                 (rec-alpha-vary new-hash (rec-with-bound-body e))
                 (rec-alpha-vary new-hash (rec-with-body e))))]
    [(fun? e)
     (let ([new-hash (hash-set hash
                               (fun-arg-id e)
                               (gensym (fun-arg-id e)))])
       (fun (rename-symbol new-hash (fun-arg-id e))
            (rec-alpha-vary new-hash (fun-body e))))]
    [(app? e) (app (rec-alpha-vary hash (app-fun-expr e))
                   (rec-alpha-vary hash (app-arg-expr e)))]
    [(tcons? e) (tcons (rec-alpha-vary hash (tcons-first e))
                       (rec-alpha-vary hash (tcons-rest e)))]
    [(tfirst? e) (tfirst (rec-alpha-vary hash (tfirst-e e)))]
    [(trest? e) (trest (rec-alpha-vary hash (trest-e e)))]
    [(istempty? e) (istempty (rec-alpha-vary hash (istempty-e e)))]
    [else 
     (error 'rec-alpha-vary "not-yet-implmented")]))




;; rename-id : hash expr -> expr
;; To create an expr identical to the one given, but with the unique id
(define (rename-id hash e)
  (make-id (rename-symbol hash (id-v e))))

;; rename-symbol : hash symbol -> symbol
;; To rename a symbol to its unique counterpart in hash
(define (rename-symbol hash sym)
  (let ([new-id (hash-ref hash sym #f)])
    (if new-id new-id (error 'rename-symbol "unbound-identifier: ~v" sym))))




;; lookup-type : symbol Env -> Type?
;; returns the type bound to an identifier.
(define (lookup-name name env)
  (type-case Env env
    [mtEnv () (error 'alpha-vary "Unbound identifier")]
    [anEnv (bound-name bound-type rest-env)
           (if (symbol=? bound-name name)
               bound-type
               (lookup-name name rest-env))]))




;; generate-constraints : symbol exp -> (listof Constraint)
;; generates all the constraints from the given s-expression
(define (generate-constraints e-id e)
  (cond
    [(num? e) (list (eqc (t-var e-id) (t-num)))]
    [(id? e) (list (eqc (t-var e-id) (t-var (id-v e))))]
    [(bool? e) (list (eqc (t-var e-id) (t-bool)))]
    [(bin-num-op? e) 
     (let ([lhs-sym (gensym 'bno-lhs-exp)]
           [rhs-sym (gensym 'bno-rhs-exp)])
       (append (generate-constraints lhs-sym (bin-num-op-lhs e))
               (generate-constraints rhs-sym (bin-num-op-rhs e))
               (list (eqc (t-var lhs-sym) (t-num))
                     (eqc (t-var rhs-sym) (t-num))
                     (eqc (t-var e-id) (t-num)))))]
    [(iszero? e)
     (let ([rhs-sym (gensym 'iszero-rhs-expr)]) 
       (append (generate-constraints rhs-sym (iszero-e e))
               (list 
                 (eqc (t-var rhs-sym) (t-num))
                 (eqc (t-var e-id) (t-bool)))))]
    [(bif? e)
     (let ([test-sym (gensym 'bif-test-expr)]
           [then-sym (gensym 'bif-then-expr)]
           [else-sym (gensym 'bif-else-expr)]
           [result-sym e-id])
       (append (generate-constraints test-sym (bif-test e))
               (generate-constraints then-sym (bif-then e))
               (generate-constraints else-sym (bif-else e))
               (list
                 (eqc (t-var test-sym) (t-bool))
                 (eqc (t-var result-sym) (t-var then-sym))
                 (eqc (t-var result-sym) (t-var else-sym)))))]
    [(with? e)
     (let ([with-expr-sym e-id]
           [bound-id-sym (with-bound-id e)]
           [bound-expr-sym (gensym 'with-bound-expr)]
           [body-expr-sym (gensym 'with-body-expr)])
       (append (generate-constraints bound-expr-sym (with-bound-body e))
               (generate-constraints body-expr-sym (with-body e))
               (list
                 (eqc (t-var bound-id-sym) (t-var bound-expr-sym))
                 (eqc (t-var with-expr-sym) (t-var body-expr-sym)))))]
    [(rec-with? e)
     (let ([rec-with-expr-sym e-id]
           [bound-id-sym (rec-with-bound-id e)]
           [bound-expr-sym (gensym 'rec-with-bound-expr)]
           [body-expr-sym (gensym 'rec-with-body-expr)])
       (append (generate-constraints bound-expr-sym (rec-with-bound-body e))
               (generate-constraints body-expr-sym (rec-with-body e))
               (list
                 (eqc (t-var bound-id-sym) (t-var bound-expr-sym))
                 (eqc (t-var rec-with-expr-sym) (t-var body-expr-sym)))))]
    [(fun? e)
     (let ([body-expr-sym (gensym 'fun-body-expr)])
       (append (generate-constraints body-expr-sym (fun-body e))
               (list
                 (eqc (t-var e-id) 
                      (t-fun (t-var (fun-arg-id e)) 
                             (t-var body-expr-sym))))))]
    [(app? e)
     (let ([app-fun-expr-sym (gensym 'app-fun-expr)]
           [app-arg-expr-sym (gensym 'app-arg-expr)])
       (append (generate-constraints app-fun-expr-sym (app-fun-expr e))
               (generate-constraints app-arg-expr-sym (app-arg-expr e))
               (list
                 (eqc (t-var app-fun-expr-sym)
                      (t-fun (t-var app-arg-expr-sym) 
                             (t-var e-id))))))]
    [(tempty? e) (list (eqc (t-var e-id) 
                            (t-list (t-var (gensym 'something)))))]
    [(tcons? e)
     (let ([first-sym (gensym 'tcons-first)]
           [rest-sym (gensym 'tcons-rest)])
       (append (generate-constraints first-sym (tcons-first e))
               (generate-constraints rest-sym (tcons-rest e))
               (list 
                 (eqc (t-var e-id) (t-var rest-sym))
                 (eqc (t-var e-id) (t-list (t-var first-sym))))))]
    [(tfirst? e)
     (let ([tfirst-type-sym (gensym 'tfirst)]
           [contained-list-sym (gensym 'tfirst-list)])
       (append (generate-constraints contained-list-sym (tfirst-e e))
               (list 
                 (eqc (t-var contained-list-sym) 
                      (t-list (t-var tfirst-type-sym)))
                 (eqc (t-var e-id) (t-var tfirst-type-sym)))))]
    [(trest? e)
     (let ([list-type-sym (gensym 'trest)]
           [list-sym (gensym 'trest-list)])
       (append (generate-constraints list-sym (trest-e e))
               (list
                 (eqc (t-var list-sym) (t-list (t-var list-type-sym)))
                 (eqc (t-var e-id) (t-var list-sym)))))]
    [(istempty? e)
     (let ([istempty-rhs-sym (gensym 'istempty-rhs-expr)])
       (append (generate-constraints istempty-rhs-sym (istempty-e e))
               (list 
                 (eqc (t-var istempty-rhs-sym) 
                      (t-list (t-var (gensym 'something))))
                 (eqc (t-var e-id) (t-bool)))))]
    [else
     (error 'generate-constraints "error in generate-constraints")]))



;; unify : (listof Constraint?) -> (listof Constraint?)
;; solves the set of given constraints.
(define (unify loc) 
  (rec-unify loc (list)))


;; rec-unify : (listof Constraint?) (listof Constraint?) 
;;              -> (listof Constraint?)
;; unify the first list of constraints with the given substitution and 
;;  return a new substitution
(define (rec-unify loc subs)
  ; (printf "Rec-Unify loc: ~v\n\n" loc)
  (cond 
    [(empty? loc) subs] ;; Return the substitution if no more constraints
    [else
     (let ([cc (first loc)])
       (cond
         [(equal? (eqc-lhs cc) (eqc-rhs cc)) 
          (rec-unify (rest loc) subs)]
         [(t-var? (eqc-lhs cc))
          (rec-unify (replace (eqc-lhs cc) (eqc-rhs cc) (rest loc))
                     (append
                       (replace (eqc-lhs cc) (eqc-rhs cc) subs)
                       (list cc)))]     
         [(t-var? (eqc-rhs cc))
          (rec-unify (replace (eqc-rhs cc) (eqc-lhs cc) (rest loc))
                     (append 
                       (replace (eqc-rhs cc) (eqc-lhs cc) subs)
                       (list (eqc (eqc-rhs cc) (eqc-lhs cc)))))]
         [(and (t-fun? (eqc-lhs cc))
               (t-fun? (eqc-rhs cc)))
          (rec-unify 
            (append (rest loc)
              (list (eqc (t-fun-arg (eqc-lhs cc)) 
                         (t-fun-arg (eqc-rhs cc)))
                    (eqc (t-fun-result (eqc-lhs cc)) 
                         (t-fun-result (eqc-rhs cc)))))
            subs)]
         [(and (t-list? (eqc-lhs cc))
               (t-list? (eqc-rhs cc)))
          (rec-unify
            (append (rest loc)
              (list (eqc (t-list-elem (eqc-lhs cc))
                         (t-list-elem (eqc-rhs cc)))))
            subs)]
         [else 
          (error 'rec-unify "unification-error ~v" (first loc))]))]))





;; find-type : (listof Constraint?) symbol? -> Type?
;; Returns the rhs of a Constraint in the list whose lhs is label
(define (find-type loc label)
  (cond
    [(empty? loc) (error 'find-type "label-doesn't-exist")]
    [(equal? (t-var-v (eqc-lhs (first loc))) label) (eqc-rhs (first loc))]
    [else (find-type (rest loc) label)]))








;; infer-type : expr? -> type?
;; infers the type of e
(define (infer-type e) 
  (find-type (unify (generate-constraints 'expr-label (alpha-vary e))) 
             'expr-label))







;; replace : Type? Type? list? -> list?
;; replace all occurences of old-type with new-type 
;;   in list unless old-type occurs in new-type
(define (replace old-type new-type loc)
  (cond
    [(empty? loc) loc]
    [else
     (cons (replace-in-constraint old-type new-type (first loc))
           (replace old-type new-type (rest loc)))]))


;; replace-in-constraint : Type? Type? Constraint? -> Constraint?
;; replace old-type with new-type in constraint 
;;   and return the new constraint. Will error if the occurs check fails.
(define (replace-in-constraint old-type new-type constraint)
  (cond
    [(and (contains-type? new-type (t-var-v old-type))
          (or (contains-type? (eqc-rhs constraint) (t-var-v old-type))
              (contains-type? (eqc-lhs constraint) (t-var-v old-type))))
     (error 'replace-in-constraint "failed-occurs-check")]
    [else 
     (eqc (replace-in-type old-type new-type (eqc-lhs constraint)) 
          (replace-in-type old-type new-type (eqc-rhs constraint)))]))


;; contains-id : Type? symbol? -> boolean
;; determine if sym occurs in type-clause
(define (contains-type? type-clause sym)
  (cond 
    [(or (t-num? type-clause)
         (t-bool? type-clause))
     false]
    [(t-list? type-clause)
     (contains-type? (t-list-elem type-clause) sym)]
    [(t-fun? type-clause)
     (or (contains-type? (t-fun-arg type-clause) sym)
         (contains-type? (t-fun-result type-clause) sym))]
    [(t-var? type-clause)
     (equal? type-clause (t-var sym))]))


;; replace-in-type : type? type? type? -> type?
;; return a new version of type-clause where old-type has been replaced
;;    with new-wtype
(define (replace-in-type old-type new-type type-clause)
  (cond 
    [(equal? old-type type-clause)
     new-type]
    [(or (t-num? type-clause)
         (t-bool? type-clause)
         (t-var? type-clause))
     type-clause]
    [(t-list? type-clause)
     (t-list (replace-in-type old-type new-type (t-list-elem type-clause)))]
    [(t-fun? type-clause)
     (t-fun (replace-in-type old-type new-type (t-fun-arg type-clause))
            (replace-in-type old-type new-type (t-fun-result type-clause)))]))







; type=?/mapping : hash hash Type Type -> Bool
; determines if types are equal modulo renaming
(define (type=?/mapping ht1 ht2 t1 t2)
  (define (teq? t1 t2)
    (type=?/mapping ht1 ht2 t1 t2))
  (cond
    [(and (t-num? t1) (t-num? t2)) true]
    [(and (t-bool? t1) (t-bool? t2)) true]
    [(and (t-list? t1) (t-list? t2))
     (teq? (t-list-elem t1) (t-list-elem t2))]
    [(and (t-fun? t1) (t-fun? t2))
     (and (teq? (t-fun-arg t1) (t-fun-arg t2))
          (teq? (t-fun-result t1) (t-fun-result t2)))]
    [(and (t-var? t1) (t-var? t2))
     (local ([define v1 ; the symbol that ht1 says that t1 maps to
               (hash-ref
                ht1 (t-var-v t1)
                (lambda ()
                  ; if t1 doesn't map to anything, it's the first
                  ; time we're seeing it, so map it to t2
                  (hash-set! ht1 (t-var-v t1) (t-var-v t2))
                  (t-var-v t2)))]
             [define v2
               (hash-ref
                ht2 (t-var-v t2)
                (lambda ()
                  (hash-set! ht2 (t-var-v t2) (t-var-v t1))
                  (t-var-v t1)))])
       ; we have to check both mappings, so that distinct variables
       ; are kept distinct. i.e. a -> b should not be isomorphic to
       ; c -> c under the one-way mapping a => c, b => c.
       (and (symbol=? (t-var-v t2) v1)
            (symbol=? (t-var-v t1) v2)))]
    [(and (Type? t1) (Type? t2)) false]
    [else (error 'type=? "either ~a or ~a is not a Type" t1 t2)]))
 
; type=? Type -> Type -> Bool
; signals an error if arguments are not variants of Type
(define ((type=? t1) t2)
  (or (type=?/mapping (make-hash) (make-hash) t1 t2)
      ; Unfortunately, test/pred simply prints false;
      ; this helps us see what t2 was.
      (error 'type=?
             "~s and ~a are not equal (modulo renaming)"
             t1 t2)))
 
(test/pred (t-var 'a)
           (type=? (t-var 'b)))
(test/pred (t-fun (t-var 'a) (t-var 'b))
           (type=? (t-fun (t-var (gensym)) (t-var (gensym)))))
(test/pred (t-fun (t-var 'a) (t-var 'b))
           (type=? (t-fun (t-var (gensym)) (t-var (gensym)))))
(test/exn ((type=? 34) 34) "not a Type")
 
; constraint-list=? : Constraint list -> Constraint list -> Bool
; signals an error if arguments are not variants of Constraint
(define ((constraint-list=? lc1) lc2)
  (define htlc1 (make-hash))
  (define htlc2 (make-hash))
  (or (andmap (lambda (c1 c2)
                (and
                 (type=?/mapping
                  htlc1 htlc2
                  (eqc-lhs c1) (eqc-lhs c2))
                 (type=?/mapping
                  htlc1 htlc2
                  (eqc-rhs c1) (eqc-rhs c2))))
              lc1 lc2)
      (error 'constraint-list=?
             "~s and ~a are not equal (modulo renaming)"
             lc1 lc2)))






;; Testing ;;



(test (alpha-vary (parse '10)) (num 10))
(test (alpha-vary (parse 'true)) (bool true))
(test (alpha-vary (parse 'false)) (bool false))
(test (alpha-vary (parse '(+ 1 2))) (bin-num-op + (num 1) (num 2)))
(test (alpha-vary (parse '(- 1 2))) (bin-num-op - (num 1) (num 2)))
(test (alpha-vary (parse '(* 1 2))) (bin-num-op * (num 1) (num 2)))
(test (alpha-vary (parse '(bif (* 1 1) (+ 40 2) (- 40 2))))
      (bif (bin-num-op * (num 1) (num 1))
           (bin-num-op + (num 40) (num 2))
           (bin-num-op - (num 40) (num 2))))

(test/exn (alpha-vary (parse '(with (x 5) y))) "unbound-identifier")

(test/exn (alpha-vary (parse '(fun (my-arg) unbound))) "unbound-identifier")
(test/exn (alpha-vary (parse '(fun (arg-id) x))) "unbound-identifier")

(test/exn (alpha-vary (parse '((fun (x) (* y 2)) 21))) "unbound-identifier")

(test (alpha-vary (parse 'tempty)) (tempty))

(test (alpha-vary (parse '(tcons 1 tempty))) (tcons (num 1) (tempty)))
(test (alpha-vary (parse '(tcons 1 (tcons 2 3)))) 
      (tcons (num 1) (tcons (num 2) (num 3))))
(test/exn (alpha-vary (parse '(tcons x tempty))) "unbound-identifier")

(test (alpha-vary (parse '(tfirst (tcons 1 tempty)))) 
      (tfirst (tcons (num 1) (tempty))))
(test/exn (alpha-vary (parse '(tfirst (tcons x tempty))))  
          "unbound-identifier")

(test (alpha-vary (parse '(trest (tcons 1 tempty)))) 
      (trest (tcons (num 1) (tempty))))
(test/exn (alpha-vary (parse '(trest (tcons x tempty))))  
          "unbound-identifier")

(test (alpha-vary (parse '(tempty? tempty))) (istempty (tempty)))
(test (alpha-vary (parse '(tempty? (tcons 1 tempty)))) 
      (istempty (tcons (num 1) (tempty))))
(test (alpha-vary (parse '40)) (num 40))
(test (alpha-vary (parse '2)) (num 2))
(test (alpha-vary (parse 'true)) (bool true))
(test (alpha-vary (parse 'false)) (bool false))
(test/exn (alpha-vary (parse 'some-id)) "unbound-identifier")
(test (alpha-vary (parse '(* 21 2))) (bin-num-op * (num 21) (num 2)))
(test (alpha-vary (parse '(iszero (* 0 5)))) 
      (iszero (bin-num-op * (num 0) (num 5))))
(test (alpha-vary (parse '(bif true (+ 40 2) (- 40 2))))
      (bif (bool true)
           (bin-num-op + (num 40) (num 2))
           (bin-num-op - (num 40) (num 2))))
(test (alpha-vary (parse '(bif (* 1 1) (+ 40 2) (- 40 2))))
      (bif (bin-num-op * (num 1) (num 1))
           (bin-num-op + (num 40) (num 2))
           (bin-num-op - (num 40) (num 2))))

(test/exn (alpha-vary (parse '(with (x 5) y))) "unbound-identifier")
(test/exn (alpha-vary (parse '(with (x (+ x 5)) y))) "unbound-identifier")
; (printf "~v\n" (alpha-vary (parse '(with (x 2) x))))
; (printf "~v\n" (alpha-vary (parse '(+ (with (x 4) x) (with (x 5) x)))))

(test/exn (alpha-vary (parse '(rec (x (+ x 5)) y))) "unbound-identifier")
; (printf "~v\n" (alpha-vary (parse '(rec (x (+ x 5)) x))))

(test/exn (alpha-vary (parse '(fun (my-arg) unbound))) "unbound-identifier")
(test/exn (alpha-vary (parse '(fun (arg-id) x))) "unbound-identifier")
; (printf "~v\n" (alpha-vary (parse '(fun (my-arg) (+ 1 my-arg)))))
; (printf "~v\n" (alpha-vary (parse '(fun (x) (* x x)))))

(test/exn (alpha-vary (parse '((fun (x) (* y 2)) 21))) "unbound-identifier")
; (printf "~v\n" (alpha-vary (parse '((fun (x) (+ x x)) 21))))
; (printf "~v\n" (alpha-vary (parse '((fun (zed) (* zed 2)) 21))))

(test (alpha-vary (parse 'tempty)) (tempty))

(test (alpha-vary (parse '(tcons 1 tempty))) (tcons (num 1) (tempty)))
(test (alpha-vary (parse '(tcons 1 (tcons 2 3)))) 
      (tcons (num 1) (tcons (num 2) (num 3))))
(test/exn (alpha-vary (parse '(tcons x tempty))) "unbound-identifier")
; (printf "~v\n" (alpha-vary (parse '(with (x 4) (tcons x x)))))

(test (alpha-vary (parse '(tfirst (tcons 1 tempty)))) 
      (tfirst (tcons (num 1) (tempty))))
(test/exn (alpha-vary (parse '(tfirst (tcons x tempty))))  
          "unbound-identifier")
; (printf "~v\n" (alpha-vary (parse '(with (var 42)
;                                     (tfirst (tcons var tempty))))))

(test (alpha-vary (parse '(trest (tcons 1 tempty)))) 
      (trest (tcons (num 1) (tempty))))
(test/exn (alpha-vary (parse '(trest (tcons x tempty))))  
          "unbound-identifier")
; (printf "~v\n" (alpha-vary (parse '(with (var 42)
;                                     (trest (tcons var tempty))))))

(test (alpha-vary (parse '(tempty? tempty))) (istempty (tempty)))
(test (alpha-vary (parse '(tempty? (tcons 1 tempty)))) 
      (istempty (tcons (num 1) (tempty))))





(test/pred (infer-type (parse '42)) (type=? (t-num)))
(test/pred (infer-type (parse '95)) (type=? (t-num)))
(test/pred (infer-type (parse 'true)) (type=? (t-bool)))
(test/pred (infer-type (parse 'false)) (type=? (t-bool)))

(test/pred (infer-type (parse '(+ 40 2))) (type=? (t-num)))
(test/exn (infer-type (parse '(+ 30 true))) "unification-error")
(test/exn (infer-type (parse '(+ true 32))) "unification-error")
(test/exn (infer-type (parse '(+ true true))) "unification-error")

(test/pred (infer-type (parse '(- 40 2))) (type=? (t-num)))
(test/exn (infer-type (parse '(- 34 true))) "unification-error")
(test/exn (infer-type (parse '(- true 234))) "unification-error")
(test/exn (infer-type (parse '(- true true))) "unification-error")

(test/pred (infer-type (parse '(* 40 2))) (type=? (t-num)))
(test/exn (infer-type (parse '(* 34 true))) "unification-error")
(test/exn (infer-type (parse '(* true 234))) "unification-error")
(test/exn (infer-type (parse '(* true true))) "unification-error")

(test/pred (infer-type (parse '(iszero (* 40 2)))) (type=? (t-bool)))
(test/exn (infer-type (parse '(iszero false))) "unification-error")
(test/exn (infer-type (parse '(iszero tempty))) "unification-error")

(test/pred (infer-type (parse '(bif true 0 1))) (type=? (t-num)))
(test/pred (infer-type (parse '(bif true true false))) (type=? (t-bool)))
(test/exn (infer-type (parse '(bif 0 0 1))) "unification-error")
(test/exn (infer-type (parse '(bif true false 1))) "unification-error")
(test/exn (infer-type (parse '(bif true 0 false))) "unification-error")

(test/pred (infer-type (parse '(with (id 5) id))) 
           (type=? (t-num)))
(test/pred (infer-type (parse '(with (id 5) true))) 
           (type=? (t-bool)))
(test/pred (infer-type (parse '(with (id true) id))) 
           (type=? (t-bool)))

(test/pred (infer-type (parse '(with (f (fun (x) x)) f)))
           (type=? (t-fun (t-var 'anything) (t-var 'anything))))
(test/pred (infer-type (parse '(fun (x) 5)))
           (type=? (t-fun (t-var 'anything) (t-num))))
(test/pred (infer-type (parse '(fun (x) (tcons x tempty))))
           (type=? (t-fun (t-var 'anything) (t-list (t-var 'anything)))))
(test/pred (infer-type (parse '(fun (x) (fun (y) x))))
           (type=? (t-fun (t-var 'anything) 
                          (t-fun (t-var 'other-anything) (t-var 'anything)))))

(test/pred (infer-type (parse '(rec (f (fun (x) (+ 1 (f x)))) (f 0))))
           (type=? (t-num)))
(test/pred (infer-type (parse '(rec (f (with (y 4)
                                             (fun (x) (f y)))) 
                                5)))
           (type=? (t-num)))
(test/pred (infer-type (parse '(rec (f (with (y 4)
                                             (fun (x) (f y)))) 
                                f)))
           (type=? (t-fun (t-num) (t-var 'something))))

(test/pred (infer-type (parse '((fun (x) x) 5)))
           (type=? (t-num)))
(test/pred (infer-type (parse '((fun (x) (iszero x)) 5)))
           (type=? (t-bool)))

(test/pred (infer-type (parse 'tempty)) (type=? (t-list (t-var 'garbage))))

(test/pred (infer-type (parse '(with (l (tcons 1 tempty)) l)))
           (type=? (t-list (t-num))))
(test/pred (infer-type (parse '(tcons 1 (tcons 2 tempty))))
           (type=? (t-list (t-num))))

(test/pred (infer-type (parse '(with (l (tcons true tempty)) l)))
           (type=? (t-list (t-bool))))
(test/pred (infer-type (parse '(tcons true (tcons false tempty))))
           (type=? (t-list (t-bool))))

(test/exn (infer-type (parse '(tcons 1 2))) "unification-error")
(test/exn (infer-type (parse '(tcons 1 (tcons true tempty)))) 
          "unification-error")

(test/pred (infer-type (parse '(tcons (tcons 1 tempty) tempty)))
           (type=? (t-list (t-list (t-num)))))

(test/pred (infer-type (parse '(tcons (tcons 1 tempty) 
                                (tcons (tcons 2 tempty) tempty))))
           (type=? (t-list (t-list (t-num)))))

(test/exn (infer-type (parse '(tcons (tcons 1 tempty) 
                               (tcons (tcons true tempty) tempty))))
           "unification-error")

(test/pred (infer-type (parse '(with (l (tcons 1 tempty)) (tempty? l))))
           (type=? (t-bool)))
(test/pred (infer-type (parse '(tempty? (tcons 1 (tcons 2 tempty)))))
           (type=? (t-bool)))

(test/pred (infer-type (parse '(with (l (tcons true tempty)) (tempty? l))))
           (type=? (t-bool)))
(test/pred (infer-type (parse '(tempty? (tcons true (tcons false tempty)))))
           (type=? (t-bool)))

(test/pred (infer-type (parse '(tfirst (tcons 1 (tcons 2 tempty)))))
           (type=? (t-num)))
(test/pred (infer-type (parse '(tfirst (trest (tcons 1 (tcons 2 tempty))))))
           (type=? (t-num)))
(test/pred (infer-type 
             (parse 
               '(tfirst (trest (trest (trest (tcons 1 (tcons 2 tempty))))))))
           (type=? (t-num)))
(test/pred (infer-type (parse '(tfirst (tcons true (tcons false tempty)))))
           (type=? (t-bool)))
(test/pred (infer-type (parse '(tfirst (tcons (tcons 1 tempty) 
                                (tcons (tcons 2 tempty) tempty)))))
           (type=? (t-list (t-num))))

(test/pred (infer-type (parse '(trest (tcons 1 (tcons 2 tempty)))))
           (type=? (t-list (t-num))))
(test/pred (infer-type (parse '(trest (tcons true (tcons false tempty)))))
           (type=? (t-list (t-bool))))
(test/pred (infer-type (parse '(trest (tcons (tcons 1 tempty) 
                                (tcons (tcons 2 tempty) tempty)))))
           (type=? (t-list (t-list (t-num)))))

(test/pred (infer-type (parse '(tfirst tempty))) 
           (type=? (t-var 'something)))
(test/pred (infer-type (parse '(trest tempty)))
           (type=? (t-list (t-var 'something))))

(define identity '(fun (x) x))
(test/pred (infer-type (parse identity))
           (type=? (t-fun (t-var 'x) (t-var 'x))))
(test/pred (infer-type (parse `(,identity 1)))
           (type=? (t-num)))
(test/pred (infer-type (parse `(with (f ,identity) (f 1))))
           (type=? (t-num)))

(define add1-fun '(fun (x) (+ x 1)))
(test/pred (infer-type (parse add1-fun))
           (type=? (t-fun (t-num) (t-num))))
(test/pred (infer-type (parse `(,add1-fun 1))) (type=? (t-num)))
(test/exn (infer-type (parse `(,add1-fun false))) "unification-error")