#lang racket

(require berkeley)

; Write a procedure which converts Roman numerals into Arabic numerals:

(define (roman-digit d)
  (cond 
    ((equal? d 'I) 1)
    ((equal? d 'V) 5)
    ((equal? d 'X) 10)
    ((equal? d 'L) 50)
    ((equal? d 'C) 100)
    ((equal? d 'D) 500)
    ((equal? d 'M) 1000)
    (else 0)
  )
)

(define (arabic r)
  (if (= (count r) 1)
    (roman-digit r)
    (if (< (roman-digit (first r)) (roman-digit (item 2 r))) 
      (- (arabic (bf r)) (roman-digit (first r)))
      (+ (roman-digit (first r)) (arabic (bf r)))
    )
  )
)

(arabic 'MCMLXXI)
; 1971
(arabic 'MLXVI)
; 1066
