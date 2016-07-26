;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ListsAndTrees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (check-temps1 temps)
  (if (empty? temps)
      #true
      (and
        (and
          (>= (first temps) 5)
          (<= (first temps) 95)
        )
        (check-temps1 (rest temps))
      )
  )
)

(define (check-temps temps low high)
  (if (empty? temps)
      #true
      (and
        (and
          (>= (first temps) low)
          (<= (first temps) high)
        )
        (check-temps (rest temps) low high)
      )
  )
)

; TODO
(define (convert digits)
  (
    implode (reverse digits)
  )
)

(define (average-price prices)
  (
    (/
      (+
        (first prices)
        (first)
      )
      (
        
      )
    )
  )
)

