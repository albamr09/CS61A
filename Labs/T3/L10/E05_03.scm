;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 5.3
;;3.52
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Initial value for accumulating function
(define sum 0)

; Define accumulating function
(define (accum x)
  (set! sum (+ x sum))
  sum
)

; Create a stream where s_i = sum of s_j for j=1 to i 
(define seq 
  (stream-map 
    accum 
    ; Sequence (stream) of integers from 1 to 20
    (stream-enumerate-interval 1 20)
  )
)

; Obtain only even "sums"
(define y (stream-filter even? seq))

; Obtain only "sums" that are multiple of 5
(define z 
  (stream-filter 
    (lambda (x) 
      ; Check if the element is multiple of 5
      (= (remainder x 5) 0)
    )
    seq
  )
)

(print (stream-ref y 7))
(display-stream z)

#| What is the value for 'sum'?

We have:

- index  1  2  3  4   5   6   7   8   9   10  11  12  13  14   15   16   17   18   19   20
- seq = {1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, 153, 171, 190, 210}

So sum = 210

|#

(newline)
(print sum)

#| What is the printed response to evaluating the stream-ref and display-stream?

So y is made up of all the even numbers in seq

y = {6, 10, 28, 36, 66, 78, 120, 136, 190, 210}

Which means (stream-ref y 7) = 136

Then z is made up of all the multiples of 5 in seq

z = {10, 15, 45, 55, 105, 120, 190, 210}


|#
