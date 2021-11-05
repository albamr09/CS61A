;; Exercise 3b.
; Write a procedure fizzbuzz that takes a number and outputs the word fizz if the number is divisible by 3,
; buzz if it's divisible by 5, 
; fizzbuzz if it's divisible by both 3 and 5, 
; and otherwise, the number itself.

(define (fizzbuzz n)
  (cond 
    ; Check if 3 is divisor of n
    ((eq? (remainder n 3) 0) 
      ; Check if 5 is divisor of n
      (if (eq? (remainder n 5) 0) 'fizzbuzz 'fizz)
    )
    ; Check if 5 is divisor of n
    ((eq? (remainder n 5) 0) 'buzz)
    (else n)
  )
)

(fizzbuzz 0)
(fizzbuzz 5)
(fizzbuzz 7)
(fizzbuzz 21)
