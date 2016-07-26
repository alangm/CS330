;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname BasicRacket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; (sum-coins number? number? number? number?) -> number?
; sum-coins produces the total amount of money, in cents.
(define (sum-coins pennies nickels dimes quarters)
  (+ pennies (* 5 nickels) (* 10 dimes) (* 25 quarters))
)

(check-expect (sum-coins 8 0 0 0) 8)
(check-expect (sum-coins 0 4 0 0) 20)
(check-expect (sum-coins 0 0 3 0) 30)
(check-expect (sum-coins 0 0 0 5) 125)
(check-expect (sum-coins 2 3 4 1) 82)

; (area-cylinder number? number?) -> number?
; returns the surface area of a cylinder
(define (area-cylinder base-radius height)
  (+ (* 2 pi base-radius height) (* 2 pi base-radius base-radius))
)

(check-within (area-cylinder 3 7) 188 1)
(check-within (area-cylinder 1 2) 18 1)

; (tax number?) -> number?
; produces the amount of tax owed, according to the specifications
(define (tax gross-pay)
  (cond
    ((<= gross-pay 240) 0)
    ((<= gross-pay 480) (* gross-pay (/ 15 100)))
    (else (* gross-pay (/ 28 100)))
  )
)

(check-expect (tax 110) 0)
(check-within (tax 250) 37 1)
(check-within (tax 520) 145 1)

; (netpay number?) -> number
; determines the net pay of an employee, at $12 an hour
(define (netpay hours-worked)
  (- (* 12 hours-worked) (tax (* 12 hours-worked)))
)

(check-within (netpay 40) 408 1)
(check-within (netpay 80) 691 1)

; (what-kind number? number? number?) -> symbol?
; tells whether a quadratic equation with the given parameters
; as 'a' 'b' and 'c', respectively, is degenerate, or produces
; one, two, or no solutions.
(define (what-kind a b c)
  (cond
    ((= a 0) 'degenerate)
    ((> (- (* b b) (* 4 a c)) 0) 'two)
    ((= (- (* b b) (* 4 a c)) 0) 'one)
    (else 'none)
  )
)

(check-expect (what-kind 0 1 1) 'degenerate)
(check-expect (what-kind 1 -2 -4) 'two)
(check-expect (what-kind 9 12 4) 'one)
(check-expect (what-kind 3 4 2) 'none)

(define-struct time (hours minutes seconds))

; (time-diff time? time?) -> number?
; returns the number of seconds from t1 to t2
(define (time-diff t1 t2)
  (abs (- (+ (* 3600 (time-hours t2)) (* 60 (time-minutes t2)) (time-seconds t2))
       (+ (* 3600 (time-hours t1)) (* 60 (time-minutes t1)) (time-seconds t1))
  ))
)

(check-expect (time-diff (make-time 1 15 15) (make-time 0 15 10)) 3605)
(check-expect (time-diff (make-time 0 0 15) (make-time 0 1 10)) 55)

(define-struct position (x y))

(define-struct circ (center radius))

(define-struct square (upper-left length))

(define-struct rect (upper-left width height))

; (area (or/c circ? square? rect?)) -> number?
; computes the area of the shape
(define (area shape)
  (cond
    ((circ? shape) (* pi (circ-radius shape) (circ-radius shape)))
    ((square? shape) (* (square-length shape) (square-length shape)))
    ((rect? shape) (* (rect-width shape) (rect-height shape)))
    (else "Not a recognized shape")
  )
)

(check-within (area (make-circ (make-position 0 0) 4)) (* pi 15) (* pi 17))
(check-expect (area (make-square (make-position 1 1) 5)) (* 5 5))
(check-expect (area (make-rect (make-position 2 2) 6 3)) (* 3 6))

; (translate-shape (or/c circ? square? rect?) number?) -> (or/c circ? square? rect?)
; produces a shape whose key position is moved by delta pixels in the x direction
(define (translate-shape shape delta)
  (cond
    ((circ? shape) (make-circ 
                    (make-position  (+ (position-x 
                                        (circ-center shape)) 
                                       delta)
                                    (position-y 
                                     (circ-center shape)))
                    (circ-radius shape)))
    ((square? shape) (make-square 
                      (make-position  (+ (position-x 
                                          (square-upper-left shape)) 
                                         delta)
                                      (position-y 
                                       (square-upper-left shape)))
                      (square-length shape)))
    ((rect? shape) (make-rect 
                    (make-position  (+ (position-x 
                                        (rect-upper-left shape)) 
                                       delta) 
                                    (position-y 
                                     (rect-upper-left shape)))
                    (rect-width shape) 
                    (rect-height shape)))
    (else "Not a recognized shape")
  )
)

(check-expect (translate-shape (make-circ (make-position 0 0) 5) 10)
                               (make-circ (make-position 10 0) 5))
(check-expect (translate-shape (make-square (make-position 0 0) 4) 10)
                               (make-square (make-position 10 0) 4))
(check-expect (translate-shape (make-rect (make-position 0 0) 3 4) 10)
                               (make-rect (make-position 10 0) 3 4))

; (in-shape (or/c circ? square? rect?) number?) position?) -> boolean?
; returns true if p is within the shape, false otherwise
(define (in-shape? shape p)
  (cond
    ((circ? shape)
      (if (and (<= (abs (- (position-x p)
                           (circ-radius shape)
                           (position-x (circ-center shape))
                         )
                   )
                   (circ-radius shape)
               )
               (<= (abs (- (position-y p)
                           (circ-radius shape)
                           (position-y (circ-center shape))
                         )
                   )
                   (circ-radius shape)
               )
           )
           #true
           #false
      )
    )
    ((square? shape)
      (if (and
            (and (>= (position-x p) (position-x (square-upper-left shape)))
                 (<= (position-x p) (+ (square-length shape) (position-x (square-upper-left shape))))
            )
            (and (>= (position-y p) (position-y (square-upper-left shape)))
                 (<= (position-y p) (+ (square-length shape) (position-y (square-upper-left shape))))
            )
          )
          #true
          #false
      )
    )
    ((rect? shape)
      (if (and
            (and (>= (position-x p) (position-x (rect-upper-left shape)))
                 (<= (position-x p) (+ (rect-width shape) (position-x (rect-upper-left shape))))
            )
            (and (>= (position-y p) (position-y (rect-upper-left shape)))
                 (<= (position-y p) (+ (rect-height shape) (position-y (rect-upper-left shape))))
            )
          )
          #true
          #false
      )
    )
    (else "Not a recognized shape")
  )
)

; inside
(check-expect (in-shape? (make-circ (make-position 0 0) 5) 
                                    (make-position 1 1)) true )
(check-expect (in-shape? (make-square (make-position 0 0) 5) 
                                      (make-position 1 1)) true )
(check-expect (in-shape? (make-rect (make-position 0 0) 5 3) 
                                    (make-position 1 1)) true )
; outside
(check-expect (in-shape? (make-circ (make-position 0 0) 5) 
                                    (make-position 15 15)) false )
(check-expect (in-shape? (make-square (make-position 0 0) 5) 
                                      (make-position 7 7)) false )
(check-expect (in-shape? (make-rect (make-position 0 0) 5 3) 
                                    (make-position 6 4)) false )