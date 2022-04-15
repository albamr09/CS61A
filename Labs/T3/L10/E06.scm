;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXERCISE 06
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Write and test two functions to manipulate nonnegative proper fractions.
; 
; The first function, fract-stream, will take as its argument a list of two nonnegative integers, the numerator and the denominator, in which the numerator is less than the denominator. It will return an infinite stream of decimal digits representing the decimal expansion of the fraction.
; 
; The second function, approximation, will take two arguments: a fraction stream and a nonnegative integer numdigits. It will return a list (not a stream) containing the first numdigits digits of the decimal expansion.
; 
; Some guidelines:

; >> (fract-stream '(1 7)) should return the stream representing the decimal
; >> expansion of 1/7, which is 0.142857142857142857...
; >> (stream-car (fract-stream '(1 7))) should return 1.
; >> (stream-car (stream-cdr (stream-cdr (fract-stream '(1 7))))) should return
; >> 2.
; >> (approximation (fract-stream '(1 7)) 4) should return (1 4 2 8).
; >> (approximation (fract-stream '(1 2)) 4) should return (5 0 0 0).

; - nums = [num, denom] from the fraction

(define (fract-stream nums)
  (let
    ; Apply division algorithm to obtain 10*num = denom * q + r, where 0 <= r < denom
    (
      (q (floor (/ (* 10 (car nums)) (cadr nums))))
      (r (remainder (* 10 (car nums)) (cadr nums)))
    )
    (cons-stream
      ; Set the quotient as the first element
      q
      ; Keep on applying on the remainder
      (fract-stream
        (cons
          r
          (cdr nums)
        )
      )
    )
  )
)

(define s (fract-stream '(1 7)))
(print (stream-ref s 0))
; 1
(print (stream-ref s 1))
; 4
(print (stream-ref s 2))
; 2
(print (stream-ref s 3))
; 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Another way
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given 1/7 = 0.142857142857142857...
; 1. Multiply by ten: n = 1.42857142857142857...
; 2. Set x = floor of n = 1
; 3. Create recursively a stream, where x is the first element
; 4. Set n = (1.42857142857142857 - 1) * 10 = 4.2857142857142857...
; 5. Go to step 2

; - nums = [num, denom] of the fraction

(define (fract-stream nums)
  (define (create-stream n)
    (let
      ; Obtain floor
      ((x (floor n)))
      ; Create stream recursively
      (cons-stream
        x
        ; Keep generating digits for the expansion
        (create-stream (* (- n x) 10))
      )
    )
  )
  ; Start by multiplying by ten, to get rid of the first zero
  ; of the decimal
  (create-stream 
    (* 
      ; Divide numerator and denominator
      (/ (car nums) (cadr nums)) 
      10
    )
  )
)

(define s (fract-stream '(1 7)))
(print (stream-ref s 0))
; 1
(print (stream-ref s 1))
; 4
(print (stream-ref s 2))
; 2
(print (stream-ref s 3))
; 8

(define (approximation stream n)
  (if (= n 0)
    '()
    (cons
      (stream-car stream)
      (approximation 
        (stream-cdr stream)
        (- n 1)
      )
    )
  )
)

(print (approximation (fract-stream '(1 7)) 4))
; (1 4 2 8)
(print (approximation (fract-stream '(1 2)) 4))
; (5 0 0 0)
