#lang plai/collector


(define heap-ptr 'uninitialized-heap-ptr)
(define current-heap 'uninitialized-current-heap-ptr)
(define old-heap-ptr 'uninitialized-heap-ptr)

;; init-allocator
;; initializes the collector.
(define (init-allocator)
  (begin
    (set! heap-ptr 0)
    (set! current-heap 1)
    (set! old-heap-ptr 0)))

;; gc:alloc-flat : any/c -> heap location (number)
;; allocates a flat value on the heap. (number, symbol, boolean, closure or empty)
(define (gc:alloc-flat val)
  (begin
    (gc:get-heap-block 2 (get-root-set))
    (heap-set! heap-ptr 'prim)
    (heap-set! (+ 1 heap-ptr) val)
    (set! heap-ptr (+ 2 heap-ptr))
    (- heap-ptr 2)))


;; gc:flat? : location -> boolean
;; determines if a location on the heap contains a flat value.
(define (gc:flat? a)
  (eq? (heap-ref a) 'prim))
 
;; gc:cons : root root -> heap location
;; allocates a cons cell on the heap
(define (gc:cons f r)
  (begin
    (gc:get-heap-block 3 (cons f (cons r (get-root-set))))
    (heap-set! heap-ptr 'cons )
    (heap-set! (+ 1 heap-ptr) (read-root f))
    (heap-set! (+ 2 heap-ptr) (read-root r))
    (set! heap-ptr (+ 3 heap-ptr))
    (- heap-ptr 3)))

;; gc:cons? : location -> boolean
;; determines if a location on the heap contains a cons cell
(define (gc:cons? a)
 (eq? (heap-ref a) 'cons))

;; gc:first
;; returns the first value in the cell if it stores a pair
(define (gc:first a) 
  (if (equal? (heap-ref a) 'pair)
      (heap-ref (+ a 1))
      (error 'first "expects address of cons")))

;; gc:rest
;; returns the rest value in the cell if it stores a pair
(define (gc:rest a) 
  (if (equal? (heap-ref a) 'pair)
      (heap-ref (+ a 2))
      (error 'first "expects address of cons")))

;; gc:set-first!
;; sets the first value in a pair
(define (gc:set-first! a f) 
  (if (equal? (heap-ref a) 'pair)
      (heap-set! (+ a 1) f)
      (error 'set-first! "expects address of cons")))

;; gc:set-rest!
;; sets the rest value in a pair
(define (gc:set-rest! a f) 
  (if (equal? (heap-ref a) 'pair)
      (heap-set! (+ a 2) f)
      (error 'set-first! "expects address of cons")))



;; current-heap-end : nothing -> location
;; To determine the last location in the current heap
(define (current-heap-end) 
  (cond
    [(= current-heap 1)
      (/ (heap-size) 2)]
    [else
      (heap-size)]))


;; gc:get-heap-block : number -> nothing
;; To verify that a free memory block of the needed size is available. If
;;   it is not, the collector is run, and we try again.
(define (gc:get-heap-block length roots)
  (cond
    [(> (+ heap-ptr length) (current-heap-end))
      (begin
        (gc:stop-and-copy roots)
        (cond 
          [(= (abs (- heap-ptr old-heap-ptr)) (/ (heap-size) 2))
            (error 'gc:get-heap-block "out of memory")]
          [else
            (gc:get-heap-block length roots)]))]))


;; gc:deref
;; dereferences the cell location, or returns the value if it is flat. 
(define (gc:deref a)
  (if (gc:flat? a)
    (heap-ref (+ 1 a))
    (error 'gc:deref "expected address of flat value")))



;; gc:closure?
(define (gc:closure? a)
 (eq? (heap-ref a) 'closure))


;; gc:closure
(define (gc:closure c fvs)
  (begin
    (gc:get-heap-block (+ 3 (length fvs)) (append fvs (get-root-set)))
    (heap-set! heap-ptr 'closure)
    (heap-set! (+ 1 heap-ptr) c)
    (heap-set! (+ 2 heap-ptr) (length fvs))
    (for ([i (in-naturals)]
          [fv (in-list fvs)])
      (heap-set! (+ 3 heap-ptr i) (read-root fv)))
    (set! heap-ptr (+ 3 (length fvs) heap-ptr))
    (- heap-ptr 3 (length fvs))))

;; gc:closure-code-ptr
(define (gc:closure-code-ptr a)
  (if (gc:closure? a)
      (heap-ref (+ 1 a))
      (error 'gc:closure-code-ptr "expects address of closure")))

;; gc:closure-env-ref
(define (gc:closure-env-ref a i)
  (if (gc:closure? a)
      (heap-ref (+ a 3 i))
      (error 'gc:closure-env-ref "expects address of closure")))
 
;; gc:closure-env-length
(define (gc:closure-env-length a)
  (if (gc:closure? a)
      (heap-ref (+ a 2))
      (error 'gc:closure-env-length "expects address of closure")))



;; gc:stop-and-copy : nothing -> nothing
;; To perform garbage collection using the stop-and-copy algorithm
(define (gc:stop-and-copy roots)
  (begin
    (set! old-heap-ptr heap-ptr)
    (gc:swap-heaps) ;; Sets heap-ptr to the beginning of the other heap
    (gc:copy-roots roots)))

;; gc:copy-roots
;; copies roots from one heap space to another
(define (gc:copy-roots roots)
  (cond
    [(empty? roots) (void)]
    [else
      (begin 
        (gc:copy-root (first roots))
        (gc:copy-roots (rest roots)))]))

;; gc:copy-root : root? -> nothing
;; copies a single root and its children to the other half of the heap.
(define (gc:copy-root root)
  (cond
    [(gc:forward? (read-root root)) (set-root! root (gc:heap-ref (read-root root)))]
    [else
      (let ([temp-loc (read-root root)])        
        (gc:copy-object (read-root root))
        (set-root! root (gc:heap-ref temp-loc)))]))

;; gc:copy-object : location -> nothing
(define (gc:copy-object loc)  
  (cond
    [(gc:flat? loc)
      (cond 
        [(> (+ 2 heap-ptr) (current-heap-end))
          (error 'gc:copy-object "out of memory")])
      (begin
        (heap-set! heap-ptr 'prim)
        (heap-set! (+ 1 heap-ptr) (gc:heap-ref (+ 1 loc)))
        (set! heap-ptr (+ 2 heap-ptr))
        (gc:forward loc (- heap-ptr 2)))]
    [(gc:cons? loc)
      (cond 
        [(> (+ 3 heap-ptr) (current-heap-end))
          (error 'gc:copy-object "out of memory")])
      (let ([cons-loc heap-ptr])
        (heap-set! heap-ptr 'cons)
        (heap-set! (+ 2 heap-ptr) (heap-ref (+ 2 loc)))
        (heap-set! (+ 1 heap-ptr) (heap-ref (+ 1 loc)))
        (gc:forward loc heap-ptr)
        (set! heap-ptr (+ 3 heap-ptr))        
        (gc:copy-object (gc:first cons-loc))
        (gc:copy-object (gc:rest cons-loc))
        (heap-set! (+ 1 cons-loc) (gc:heap-ref (heap-ref (+ 1 cons-loc))))
        (heap-set! (+ 2 cons-loc) (gc:heap-ref (heap-ref (+ 2 cons-loc)))))]
    [(gc:closure? loc)
      (cond 
        [(> (+ heap-ptr 3 (gc:closure-env-length loc)) (current-heap-end))
          (error 'gc:copy-object "out of memory")])
      (let ([clos-loc heap-ptr])
        (heap-set! heap-ptr 'closure)
        (heap-set! (+ 1 heap-ptr) (gc:closure-code-ptr loc))
        (heap-set! (+ 2 heap-ptr) (gc:closure-env-length loc))
        (for ([i (gc:closure-env-length heap-ptr)])
          (heap-set! (+ i 3 heap-ptr) (heap-ref (+ i 3 loc))))
        (gc:forward loc heap-ptr)
        (set! heap-ptr (+ 3 (gc:closure-env-length heap-ptr) heap-ptr))
        (for ([i (gc:closure-env-length clos-loc)])
          (gc:copy-object (gc:closure-env-ref clos-loc i)))
        (for ([i (gc:closure-env-length clos-loc)])
          (heap-set! (+ i 3 clos-loc)
                     (gc:heap-ref (heap-ref (+ i 3 clos-loc))))))]))

;; gc:forward
(define (gc:forward loc new-loc)
  (begin
    (heap-set! loc 'forward)
    (heap-set! (+ 1 loc) new-loc)))

;; gc:forward? : location -> boolean
(define (gc:forward? loc)
  (eq? (heap-ref loc) 'forward))

;; gc:heap-ref
(define (gc:heap-ref loc)
  (cond
    [(gc:forward? loc) (begin (heap-ref (+ 1 loc)))]
    [else (begin (heap-ref loc))]))


;; gc:swap-heaps
;;switches which half of the heap to use for allocation.
(define (gc:swap-heaps)
  (cond
    [(= current-heap 1)
      (begin
        (set! heap-ptr (/ (heap-size) 2))
        (set! current-heap 2))]
    [else
      (begin
        (set! heap-ptr 0)
        (set! current-heap 1))]))

;; gc:object-size : location -> number
;;determines the size of the object at the given location
(define (gc:object-size loc)
  (cond
    [(gc:flat? loc) 2]
    [(gc:cons? loc) 3]
    [(gc:closure? loc) (+ 3 (heap-ref (+ 2 loc)))]
    [else (error 'gc:object-size "expects address of flat, cons, or closure")]))

