#lang plai/mutator

(allocator-setup "markAndSweepCollector.rkt" 20)




;; These will take four spaces, leaving 5 
;; (+ 1 to mark the end of memory)
(let ([x 'x][y 'y]))

;; This will allocate the 1 and 2, leaving only one
;; space. If 1 and 2 weren't being counted as roots,
;; The collector would blow up
(cons 1 2)

(printf "Success!")







