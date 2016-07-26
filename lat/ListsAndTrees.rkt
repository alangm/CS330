;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ListsAndTrees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



;; check-temps1 : (listof number) -> boolean
;; Consumes a list of temperature measures and checks whether all
;; measurements are between 5 and 95 degrees celsius (inclusively.)
(define (check-temps1 temps)
  (if
    (empty? temps)
    true
    (and
      (and (>= (first temps) 5) 
            (<= (first temps) 95))
      (check-temps1 (rest temps)))))

;; Check bounds
(check-expect (check-temps1 (list 5 95)) true)
(check-expect (check-temps1 (list 6 10 30 50 70 94)) true)
(check-expect (check-temps1 (list 4 20)) false)
(check-expect (check-temps1 (list 10 96)) false)
;; Empty list
(check-expect (check-temps1 (list)) true)





;; check-temps : (listof number) number number -> boolean
;; Consumes a list of temperature measures and checks whether all
;; measurements are between low and high degrees celsius (inclusively.)
(define (check-temps temps low high)
  (if
    (empty? temps)
    true
      (and
        (and (>= (first temps) low) 
             (<= (first temps) high))
        (check-temps (rest temps) low high))))

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
    (+ (first digits) 
       (* (convert (rest digits)) 10))))

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
    (/ (sum-list prices) (length prices))))

;; sum-list : (listof number) -> number
;; A helper function. It consumes a list of numbers and returns the sum of all of them.
(define (sum-list list)
  (if (empty? list)
  0
  (+ (first list) (sum-list (rest list)))))

;; average-price tests
(check-expect (average-price (list 0)) 0)
(check-expect (average-price (list 1 2 3 4 5 6 7)) 4)
;; Empty list
(check-expect (average-price (list)) 0)





;; convertFC : (listof number) -> (listof number)
;; Converts a list of of Fahrenheit measurements to a list of Celsius measurements.
(define (convertFC temps)
  (if
    (empty? temps)
    empty
    (cons (* (/ 5 9) (- (first temps) 32))
          (convertFC (rest temps)))))

;; convertFC tests
(check-expect (convertFC (list 32)) (list 0))
(check-expect (convertFC (list 32 41 50 104)) (list 0 5 10 40))
;; Empty list
(check-expect (convertFC (list)) empty)





;; eliminate-exp : number (listof number) -> (listof number)
;; Eliminates from lotp all toys whose price is greater than ua
(define (eliminate-exp ua lotp)
  (if
    (empty? lotp)
    empty
    (if 
      (<= (first lotp) ua) 
      (cons (first lotp) (eliminate-exp ua (rest lotp)))
      (eliminate-exp ua (rest lotp)))))

;; eliminate-exp tests
(check-expect (eliminate-exp 10 (list 0 1 4 7 10 14 20))
                                (list 0 1 4 7 10))
(check-expect (eliminate-exp 0 (list 1 4 7 10 14 20))
                                empty)
;; Empty list
(check-expect (eliminate-exp 0 empty) empty)





;; suffixes : list -> (listof list)
;; Produces a list of a suffixes of l
(define (suffixes l)
  (if 
    (empty? l)
    (list empty)
    (cons l (suffixes (rest l)))))

;; suffixes tests
(check-expect (suffixes (list 10)) (list (list 10) empty))
(check-expect (suffixes (list 10 9 8 7)) (list (list 10 9 8 7)
                                               (list 9 8 7)
                                               (list 8 7)
                                               (list 7)
                                               empty))
(check-expect (suffixes (list 'a 'b 'c 'd)) (list (list 'a 'b 'c 'd) 
                                            (list 'b 'c 'd) 
                                            (list 'c 'd) 
                                            (list 'd) 
                                            empty))
;; Empty list
(check-expect (suffixes empty) (list empty))





;; struct unknown ()
;; Represents an unknown ancestor
(define-struct unknown ())





;; struct person (string number symbol (or/c unknown person) (or/c unknown person))
;; Represents a person
(define-struct person (name birthyear eyecolor father mother))





;; count-persons : (or/c unknown person) -> number
;; Returns the number of people in a family tree.
(define (count-persons ftree)
  (if
    (unknown? ftree)
    0
    (+ 1
       (count-persons (person-father ftree)) 
       (count-persons (person-mother ftree)))))

;; testing count-persons
(define val (make-person "Val" 1930 'brown (make-unknown) (make-unknown)))
(define glena (make-person "Glena" 1925 'blue (make-unknown) (make-unknown)))
(define cindy (make-person "Cindy" 1957 'hazel val (make-unknown)))
(define joseph (make-person "Joseph" 1954 'brown (make-unknown) glena))
(define alan (make-person "Alan" 1990 'brown joseph cindy))

(check-expect (count-persons alan) 5)
(check-expect (count-persons joseph) 2)
(check-expect (count-persons cindy) 2)
(check-expect (count-persons glena) 1)
(check-expect (count-persons val) 1)
(check-expect (count-persons (make-unknown)) 0)





;; average-age : (or/c unknown person) -> number
;; Returns the average age of all the people in the family tree.
;; (Assume the current year is 2016.)
(define (average-age ftree)
  (if
    (unknown? ftree)
    0
    (/ (sum-ages ftree)
       (count-persons ftree))))

;; sum-ages : (or/c unknown person) -> number
;; A helper function. Adds the ages of all people in a tree
(define (sum-ages ftree)
  (if
    (unknown? ftree)
    0
    (+ (- 2016 (person-birthyear ftree))
       (+ (sum-ages (person-father ftree)) (sum-ages (person-mother ftree))))))

;; average-age tests
;; val=86, glena=91, cindy=59, joseph=62, alan=25
(check-within (average-age alan) 64 65)
(check-within (average-age joseph) 76 77)
(check-within (average-age cindy) 72 73)
(check-expect (average-age glena) 91)
(check-expect (average-age val) 86)
(check-expect (average-age (make-unknown)) 0)





;; eye-colors : (or/c unknown person) -> (listof symbol)
;; Produces a list of all eye colors in family tree. 
(define (eye-colors ftree)
  (if
    (unknown? ftree)
    empty
    (cons (person-eyecolor ftree) 
          (append (eye-colors (person-father ftree))
                  (eye-colors (person-mother ftree))))))

;; eye-colors tests
(check-expect (eye-colors alan) (list 'brown 'brown 'blue 'hazel 'brown))
(check-expect (eye-colors joseph) (list 'brown 'blue))
(check-expect (eye-colors cindy) (list 'hazel 'brown))
(check-expect (eye-colors glena) (list 'blue))
(check-expect (eye-colors val) (list 'brown))
(check-expect (eye-colors (make-unknown)) empty)




