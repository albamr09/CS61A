;; Exercise 3a.
; Write a procedure can-drive that takes the age of a person as an argument. 
; If the age is below 16, return the sentence (not yet). 
; Otherwise, return the sentence (Good to go). 

(define (can-drive age)
  (if (< age 16) '(Not yet)
    '(Good to go))
)

(can-drive 15)
(can-drive 16)
(can-drive 36)
