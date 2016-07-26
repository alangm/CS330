#lang lazy

(define print-only-errors #f)
(define (test l r)
  (if (equal? l r)
      (if print-only-errors
          ()
          (printf "Test Passed~n"))
      (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)))


;bil builds a sequence of integers starting with the given integer.
(define (bil n) (cons n (bil (+ n 1))))

;take-while removes a prefix of the list based upon the criterion function p.
;(take-while p l) → (listof any/c)
;  p : (any/c . -> . boolean?)
;  l : (listof any/c)
(define (take-while p l) (if(empty? l)
                            `()
                            (if (p (first l))
                             (cons (first l) (take-while p (rest l)))
                             empty
                          )))


(test (take-while odd? '()) '())
(test (take-while odd? '(2 4 6)) '())
(test (take-while odd? (list 1 3 4))
      (list 1 3))
(test (take-while (lambda(x) (< x 8)) (bil 0))
      (list 0 1 2 3 4 5 6 7))




;builds an infinite list where the value of each index = f(index)
;(build-infinite-list f) → (listof any/c)
;  f : (exact-nonnegative-integer? . -> . any/c)
(define (build-infinite-list f) (map f (bil 0)));

(test (take-while (lambda(x) (< x 200)) (build-infinite-list (lambda(x) (* 2 x)))) '(0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118 120 122 124 126 128 130 132 134 136 138 140 142 144 146 148 150 152 154 156 158 160 162 164 166 168 170 172 174 176 178 180 182 184 186 188 190 192 194 196 198))
(test (take-while (lambda(x) (< x 50)) (build-infinite-list (lambda(x) (/ x 2)))) '(0 1/2 1 3/2 2 5/2 3 7/2 4 9/2 5 11/2 6 13/2 7 15/2 8 17/2 9 19/2 10 21/2 11 23/2 12 25/2 13 27/2 14 29/2 15 31/2 16 33/2 17 35/2 18 37/2 19 39/2 20 41/2 21 43/2 22 45/2 23 47/2 24 49/2 25 51/2 26 53/2 27 55/2 28 57/2 29 59/2 30 61/2 31 63/2 32 65/2 33 67/2 34 69/2 35 71/2 36 73/2 37 75/2 38 77/2 39 79/2 40 81/2 41 83/2 42 85/2 43 87/2 44 89/2 45 91/2 46 93/2 47 95/2 48 97/2 49 99/2))





;; ---------- prime? ----------
;; prime? : exact-positive-integer -> boolean
;;
;; Returns true if n is prime
;;
;; a number with no divisors other than 1 and itself is prime
;; a number with no prime divisors is also prime
 
(define prime?
  (lambda (n)
    (cond
      [(< n 2) false]
      [(= n 2) true]
      [(even? n) false]
      [else
        (let prime-test ((d 3))
          (cond
            [(> (* d d) n) true]
            [(divides? n d) false]
            [else (prime-test (+ d 2))]))])))

;; helper function
(define divides?
  (lambda (a b)
    (= (remainder a b) 0)))



;; ---------- primes ----------
;; primes : (listof exact-positive-integer)
;;
;; The list of all primes

(define primes
  (filter (lambda (n) (prime? n)) (build-infinite-list (lambda (n) (+ 1 n)))))



;; ---------- prime?/fast ----------
;; prime?/fast : exact-positive-integer -> boolean
;;
;; Returns true if n is prime, but tests only prime factors from primes/fast.
(define (prime?/fast n)
  (cond
    [(< n 2) false]
    [(= n 2) true]
    [else 
      (empty? 
        (filter (lambda (x) (= (modulo n x) 0))
                (take-while (lambda (x) (<= (* 2 x) n)) 
                            primes/fast)))]))

;; ---------- primes/fast ----------
;; primes/fast : (listof exact-positive-integer)
;;
;; The list of all primes constructed with prime?/fast.
(define primes/fast
  (filter (lambda (n) (prime?/fast n)) (build-infinite-list (lambda (n) (+ 1 n)))))

;; ---------- Testing prime? ----------
(test (prime? 2) true)
(test (prime? 3) true)
(test (prime? 5) true)
(test (prime? 7) true)
(test (prime? 11) true)
(test (prime? 13) true)
(test (prime? 19) true)
(test (prime? 23) true)
(test (prime? 29) true)
(test (prime? 113) true)
(test (prime? 2467) true)
(test (prime? 3463) true)
(test (prime? 5741) true)

(test (prime? -1) false)
(test (prime? 1) false)
(test (prime? 0) false)
(test (prime? 4) false)
(test (prime? 8) false)
(test (prime? 10) false)
(test (prime? 50) false)
(test (prime? 75) false)
(test (prime? 1232) false)
(test (prime? 1240) false)

;; ---------- Testing primes ----------
(test (list-ref primes 0) 2)
(test (list-ref primes 1) 3)
(test (list-ref primes 2) 5)
(test (list-ref primes 3) 7)
(test (list-ref primes 4) 11)
(test (list-ref primes 5) 13)
(test (list-ref primes 6) 17)
(test (list-ref primes 7) 19)
(test (list-ref primes 8) 23)
(test (list-ref primes 9) 29)
(test (list-ref primes 10) 31)
(test (list-ref primes 167) 997)
(test (list-ref primes 309) 2053)
(test (list-ref primes 499) 3571)

;; ---------- Testing prime?/fast ----------
(test (prime?/fast -1) false)
(test (prime?/fast 0) false)
(test (prime?/fast 1) false)
(test (prime?/fast 2) true)
(test (prime?/fast 3) true)
(test (prime?/fast 5) true)
(test (prime?/fast 7) true)
(test (prime?/fast 11) true)
(test (prime?/fast 13) true)
(test (prime?/fast 19) true)
(test (prime?/fast 23) true)
(test (prime?/fast 29) true)
(test (prime?/fast 113) true)
(test (prime?/fast 2467) true)
(test (prime?/fast 3463) true)
(test (prime?/fast 5741) true)

(test (prime?/fast 4) false)
(test (prime?/fast 8) false)
(test (prime?/fast 10) false)
(test (prime?/fast 50) false)
(test (prime?/fast 75) false)
(test (prime?/fast 1232) false)
(test (prime?/fast 1240) false)

;; ---------- Testing primes/fast ----------
(test (list-ref primes/fast 0) 2)
(test (list-ref primes/fast 1) 3)
(test (list-ref primes/fast 2) 5)
(test (list-ref primes/fast 3) 7)
(test (list-ref primes/fast 4) 11)
(test (list-ref primes/fast 5) 13)
(test (list-ref primes/fast 6) 17)
(test (list-ref primes/fast 7) 19)
(test (list-ref primes/fast 8) 23)
(test (list-ref primes/fast 9) 29)
(test (list-ref primes/fast 10) 31)
(test (list-ref primes/fast 167) 997)
(test (list-ref primes/fast 309) 2053)
(test (list-ref primes/fast 499) 3571)


;build-table builds a table rows X cols and the value at each cell = (f rows cols)
;(build-table rows cols f) → (vectorof (vectorof any/c))
;  rows : exact-positive-integer?
;  cols : exact-positive-integer?
;  f : (exact-nonnegative-integer? exact-nonnegative-integer? . -> . any/c)

(define (build-table rows cols f) (build-vector rows (lambda(x) (build-vector cols (lambda(y)(f x y))))))

(test (build-table 5 5 (lambda(x y) (* x y))) #(#(0 0 0 0 0) #(0 1 2 3 4) #(0 2 4 6 8) #(0 3 6 9 12) #(0 4 8 12 16)) )

(test (build-table 3 7 (lambda(x y) (+ x y))) #(#(0 1 2 3 4 5 6) #(1 2 3 4 5 6 7) #(2 3 4 5 6 7 8)))





;; lcs-length : string string -> exact-nonnegative-integer
;; Computes the length of the longest common subsequence of two strings s1 and s2.
(define (lcs-length s1 s2)
  (define 
    t 
    (build-table 
      (+ (string-length s1) 1) (+ (string-length s2) 1)
      (λ (r c)
        (cond
          [(or (= r 0) (= c 0)) 0]
          [else
           (if (char=? (string-ref s1 (- r 1)) (string-ref s2 (- c 1)))
               (+ (vector-ref (vector-ref t (- r 1)) (- c 1)) 1)
               (max (vector-ref (vector-ref t (- r 1)) c)
                    (vector-ref (vector-ref t r) (- c 1))))]))))
  (vector-ref (vector-ref t (string-length s1)) (string-length s2)))


(test (lcs-length "string" "string") 6)
(test (lcs-length "abcdefgh" "ijklmnop") 0)
(test (lcs-length "artist" "artsy") 4)
(test (lcs-length "computerscience" "compsci") 7)
(test (lcs-length "monkey" "money") 5)
(test (lcs-length "abababab" "aaaaaaaa") 4)
(test (lcs-length "abababab" "bbbbbbbb") 4)




