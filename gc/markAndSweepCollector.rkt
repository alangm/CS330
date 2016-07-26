#lang plai/collector

(define heap-ptr 'uninitialized-heap-ptr)
(define free-ptr 'uninitialized-free-ptr)


;; init-allocator
;; initializes the collector.
(define (init-allocator)
  (begin
    (set! heap-ptr 0)
    (set! free-ptr 0)
    (heap-set! (- (heap-size) 1) 'end)
    (for ([i (- (heap-size) 1)])
      (heap-set! i (+ i 1)))))

;; gc:alloc-flat : any/c -> heap location (number)
;; allocates a flat value on the heap. (number, symbol, boolean, closure or empty)
(define (gc:alloc-flat val)
  (begin
    (gc:get-heap-block free-ptr 2 (get-root-set))
    (heap-set! heap-ptr 'prim)
    (heap-set! (+ 1 heap-ptr) val)
    (set! heap-ptr (+ 2 heap-ptr))
    (- heap-ptr 2)))
 
;; gc:cons : root root -> heap location
;; allocates a cons cell on the heap
(define (gc:cons f r)
 (begin
   (gc:get-heap-block free-ptr 3 (cons f (cons r (get-root-set))))
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


;; gc:flat? : location -> boolean
;; determines if a location on the heap contains a flat value.
(define (gc:flat? a)
 (eq? (heap-ref a) 'prim))

;; gc:deref
;; dereferences the cell location, or returns the value if it is flat. 
(define (gc:deref a)
  (if (gc:flat? a)
    (heap-ref (+ 1 a))
    (error 'gc:deref "expected address of flat value")))

(define (gc:free-block-is-contiguous start length)
  (cond 
    [(eq? (heap-ref start) 'end)
      #f]
    [(= length 1)
      #t]
    [else
      (and (= (heap-ref start) (+ 1 start))
           (gc:free-block-is-contiguous (heap-ref start) (- length 1)))]))

(define (gc:mark-reachable roots)
  (cond 
    [(empty? roots)]
    [else
      (begin
        (gc:mark-reachable-from-object (read-root (first roots)))
        (gc:mark-reachable (rest roots)))]))

(define (gc:mark-reachable-from-object loc)
  (cond
    [(gc:flat? loc)
      (gc:mark loc)]
    [(gc:cons? loc)
      (begin
        (gc:mark loc)
        (gc:mark-reachable-from-object (heap-ref (+ 1 loc)))
        (gc:mark-reachable-from-object (heap-ref (+ 2 loc))))]
    [(gc:closure? loc)
      (begin
        (gc:mark loc)
        (for ([i (heap-ref (+ 2 loc))])
          (gc:mark-reachable-from-object (heap-ref (+ 3 loc i)))))]))

;; gc:mark : location -> nothing
;; marks an object
(define (gc:mark loc)
  (cond
    [(gc:flat? loc) (heap-set! loc 'marked-prim)]
    [(gc:cons? loc) (heap-set! loc 'marked-cons)]
    [(gc:closure? loc) (heap-set! loc 'marked-closure)]
    [else
      (error 'gc:mark "expects address of flat, cons, or closure")]))

; mark types...

(define (gc:marked-flat? a)
  (eq? (heap-ref a) 'marked-prim))

(define (gc:marked-cons? a)
  (eq? (heap-ref a) 'marked-cons))

;; gc:marked? : location -> boolean
;; is an object marked?
(define (gc:marked? loc)
  (or (gc:marked-flat? loc)
      (gc:marked-cons? loc)
      (gc:marked-closure? loc)))

;; gc:unmark : location -> nothing
;; unmarks an object
(define (gc:unmark loc)
  (cond
    [(gc:marked-flat? loc) (heap-set! loc 'prim)]
    [(gc:marked-cons? loc) (heap-set! loc 'cons)]
    [(gc:marked-closure? loc) (heap-set! loc 'closure)]
    [else (error 'gc:unmark "expected address of marked flat, cons, or closure")]))

;; gc:unmarked? : location -> boolean
;; determines if an object is unmarked
(define (gc:unmarked? loc)
  (or (gc:flat? loc)
      (gc:cons? loc)
      (gc:closure? loc)))

;; gc:closure?
(define (gc:closure? a)
 (eq? (heap-ref a) 'closure))

;; gc:marked-closure?
(define (gc:marked-closure? a)
 (eq? (heap-ref a) 'marked-closure))

;; gc:closure
(define (gc:closure c fvs)
  (begin
    (gc:get-heap-block free-ptr (+ 3 (length fvs)) (append fvs (get-root-set)))
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

;; gc:reset-free-ptr : location -> nothing
;; sets the free-ptr to location if it isn't pointing somewhere already
(define (gc:reset-free-ptr loc)
  (cond [(= free-ptr -1) (set! free-ptr loc)]))

;; gc:object-size : location -> number
;; size of an object
(define (gc:object-size loc)
  (cond
    [(gc:marked-flat? loc) 2]
    [(gc:marked-cons? loc) 3]
    [(gc:marked-closure? loc) (+ 3 (heap-ref (+ 2 loc)))]
    [else (error 'gc:object-size "expected address of marked object")]))

;; gc:next-unmarked-cell : location -> location
;; location of the next unmarked cell
(define (gc:next-unmarked-cell loc)
  (cond
    [(gc:marked? (+ 1 loc))
     (gc:next-unmarked-cell (+ loc (gc:object-size (+ 1 loc))))]
    [else
      (begin
        (cond [(gc:unmarked? (+ 1 loc)) (set! heap-ptr #t)])
        (+ 1 loc))]))


(define (gc:get-heap-block start length roots)
  (cond
    [(eq? (heap-ref start) 'end)
      (begin
        ;; the heap ptr to stores whether or not anything
        ;; was actually freed during the sweep. If so, we can try
        ;; again, if not, then we're really out of memory.
        (set! heap-ptr #f)
        (gc:mark-and-sweep roots) ;; Run the collector
        (cond
          [(eq? heap-ptr #t) ;; If anything was freed, try again. 
            (gc:get-heap-block free-ptr length roots)]
          [else ;; Nothing changed, so we're really out of memory now
            (error 'gc:get-heap-block "out of memory")]))]
    [(gc:free-block-is-contiguous start length)
      (begin
        (set! heap-ptr start)
        (set! free-ptr (heap-ref (- (+ start length) 1))))]
    [else
      (gc:get-heap-block (heap-ref start) length roots)]))



;; gc:sweep : number -> nothing
;; frees all unmarked memory
(define (gc:sweep start)
  (cond 
    [(eq? (heap-ref start) 'end) (gc:reset-free-ptr start)]
    [(gc:marked? start) (begin
                          (gc:sweep (+ start (gc:object-size start)))
                          (gc:unmark start))]
    [else
      (begin
        (gc:reset-free-ptr start)
        (heap-set! start (gc:next-unmarked-cell start))
        (gc:sweep (+ 1 start)))]))



;; gc:mark-and-sweep
;; performs mark and sweep garbage collection
(define (gc:mark-and-sweep roots)
  (begin
    (printf "Running Collector...\n")
    (gc:mark-reachable roots)
    (set! free-ptr -1)
    (gc:sweep 0)))