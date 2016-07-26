;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HigherOrderFunctions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Alan Moody
;; 1/27/2016



;; check-temps1 : (listof number) -> boolean
;; Consumes a list of temperature measures and checks whether all
;; measurements are between 5 and 95 degrees celsius (inclusively.)
(define (check-temps1 temps)
  (=
    (length temps)
    (length
      (filter
        (λ
          (x)
          (and
            (>= (first temps) 5) 
            (<= (first temps) 95)))
        temps))))

;; Check bounds
(check-expect (check-temps1 (cons 5 (cons 95 empty))) true)
(check-expect (check-temps1 (cons 6 (cons 10 (cons 30 (cons 50 (cons 70 (cons 94 empty))))))) true)
(check-expect (check-temps1 (cons 4 (cons 20 empty))) false)
;; Empty list
(check-expect (check-temps1 empty) true)





;; check-temps : (listof number) number number -> boolean
;; Consumes a list of temperature measures and checks whether all
;; measurements are between low and high degrees celsius (inclusively.)
(define (check-temps temps low high)
  (=
    (length temps)
    (length
      (filter
        (λ
          (x)
          (and
            (>= (first temps) low) 
            (<= (first temps) high)))
        temps))))

;; check-temps tests
(check-expect (check-temps (list 0 20 60 100) 0 100) true)
(check-expect (check-temps (list 1 99) 0 100) true)
(check-expect (check-temps (list 4 40 50 60 96) 5 95) false)
(check-expect (check-temps (list 0) 5 95) false)
;; Empty list
(check-expect (check-temps (list) 5 95) true)





;; convert : (listof number) -> number
;; Consumes a list of digits (numbers between 0 and 9) and produces
;; the corresponding number.
(define (convert digits)
  (if
    (empty? digits)
    0
    (+
      (first digits)
      (convert
        (map
          (λ (x) (* x 10))
          (rest digits))))))

;; convert tests
(check-expect (convert (cons 1 (cons 2 (cons 3 empty)))) 321)
(check-expect (convert (list 5 4 3 2 1)) 12345)
(check-expect (convert (list 0)) 0)
;; Empty list
(check-expect (convert (list)) 0)





;; average-price : (listof number) -> number
;; Consumes a list of toy prices and computes the average price of a toy.
(define (average-price prices)
  (if
    (empty? prices)
    0
    (/ (foldr + 0 prices) (length prices))))

;; average-price tests
(check-expect (average-price (list 0)) 0)
(check-expect (average-price (list 1 2 3 4 5 6 7)) 4)
;; Empty list
(check-expect (average-price (list)) 0)





;; fToC : number -> number
;; converts a fahrenheit temperature to celsius
(define (fToC temp)
  (* (/ 5 9) (- temp 32)))

;; convertFC : (listof number) -> (listof number)
;; Converts a list of of Fahrenheit measurements to a list of Celsius measurements.
(define (convertFC temps)
  (map fToC temps))

;; convertFC tests
(check-expect (convertFC (list 32)) (list 0))
(check-expect (convertFC (list 32 41 50 104)) (list 0 5 10 40))
;; Empty list
(check-expect (convertFC (list)) empty)

  



;; eliminate-exp : number (listof number) -> (listof number)
;; Eliminates from lotp all toys whose price is greater than ua
(define (eliminate-exp ua lotp)
  (filter (λ (x) (<= x ua)) lotp))

;; eliminate-exp tests
(check-expect (eliminate-exp 10 (list 0 1 4 7 10 14 20)) (list 0 1 4 7 10))
(check-expect (eliminate-exp 0 (list 1 4 7 10 14 20)) empty)
;; Empty list
(check-expect (eliminate-exp 0 empty) empty)





;; compose-func : function function -> function
;; Returns the composition of before and after.
(define (compose-func after before)
  (λ
    (x)
    (after (before x))))

;; compose-func tests
(check-expect ((compose-func add1 add1) 0) 2)
(check-expect ((compose-func / /) 10) 10)
(check-expect ((compose-func +  -) 10) -10)



  

;; flatten : (listof (listof number)) -> (listof number)
;; Produces a list of all the numbers in the elements of lolon.
(define (flatten lolon)
  (if
    (empty? lolon)
    empty
    (append
      (first lolon)
      (flatten (rest lolon)))))

;; flatten tests
(check-expect (flatten (list (list 0 1) (list 2 3))) (list 0 1 2 3))
(check-expect (flatten (list (list 0 1 3) (list 2 3 4))) (list 0 1 3 2 3 4))
;; Empty
(check-expect (flatten empty) empty)
(check-expect (flatten (list empty)) empty)





;; flatten-foldr : (listof (listof number)) -> (listof number)
;; Produces a list of all the numbers in the elements of lolon.
(define (flatten-foldr lolon)
  (foldr append empty lolon))

;; flatten-foldr tests
(check-expect (flatten-foldr (list (list 1 2) (list 3 4))) (list 1 2 3 4))
(check-expect (flatten-foldr (list (list 0 1) (list 1 2 3))) (list 0 1 1 2 3))
;; Empty
(check-expect (flatten-foldr empty) empty)
(check-expect (flatten-foldr (list empty)) empty)





;; bucket : (listof number) -> (listof (listof number))
;; Returns a list of sublists of adjacent equal numbers.
(define (bucket lon)
  (foldr
    (λ
      (element rest-val)
      (cond 
        [(empty? rest-val)
          (list (list element))]
        [(= element (first (first rest-val)))
          (cons (cons element (first rest-val)) (rest rest-val))]
        [else (cons (list element) rest-val)]))
  empty lon))

;; bucket tests:
(check-expect (bucket (list 0 0 0 1 1 2 2)) (list (list 0 0 0) 
                                                (list 1 1) 
                                                (list 2 2)))
(check-expect (bucket (list 0 1)) (list (list 0) (list 1)))
;; Empty
(check-expect (bucket empty) empty)



