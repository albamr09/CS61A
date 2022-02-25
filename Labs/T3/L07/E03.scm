(load "../../../lib/obj.scm")

; For a statistical project you need to compute lots of random numbers in 
; various ranges. (Recall that (random 10) returns a random number between 0 
; and 9.) Also, you need to keep track of how many random numbers are computed in 
; each range. You decide to use object-oriented programming. Objects of the class 
; random-generator will accept two messages: number and count. The message number 
; means "give me a random number in your range" while count means "how many number 
; requests have you had?" The class has an instantiation argument that specifies the 
; range of random numbers for this object, so:

; will create an object such that (ask r10 'number) will return a random number between 0 
; and 9, while (ask r10 'count) will return the number of random numbers r10 has created.

(define-class (random-generator upper-limit)
  (instance-vars (count 0))
  (method (number)
    ; Update count
    (set! count (+ count 1))
    ; Generate random number between 0 and 9
    (random upper-limit)
  )
)

(define r10 (instantiate random-generator 10))
(ask r10 'number)
(ask r10 'count)
; 1
(ask r10 'number)
(ask r10 'count)
; 2
(ask r10 'number)
(ask r10 'count)
; 3
