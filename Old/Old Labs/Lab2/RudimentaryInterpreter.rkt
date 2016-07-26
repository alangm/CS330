#lang plai

(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])
 
(define-type WAE
  [num (n number?)]
  [binop (op procedure?)
         (lhs WAE?)
         (rhs WAE?)]
  [with (lob (listof Binding?)) (body WAE?)]
  [id (name symbol?)])

(define op-table
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)))

(define (lookup-op s)
  (cadr (assoc s op-table)))

(test (lookup-op '+) +)
(test (lookup-op '-) -)
(test (lookup-op '*) *)
(test (lookup-op '/) /)
;(test (lookup-op '$) #f)
;(test (lookup-op '@) #f)
;(test (lookup-op '!) #f)




