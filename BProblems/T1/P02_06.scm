; Find if a number is a prime

(define (divides? a b) 
  ; Test if a | b
  (= (remainder b a) 0)
)

(define (find-divisor n test-divisor)
  (cond 
    ; If test-divisor > sqrt(n), then return n
    ; as the smallest divisor of itself
    ((> (square test-divisor) n) n)
    ; If test-divisor divides n, then it is its smallest divisor
    ((divides? test-divisor n) test-divisor)
    ; Else continue searching and update test-divisor adding 1
    (else (find-divisor n (+ test-divisor 1)))
  )
)

; Get the smallest divisor of n
(define (smallest-divisor n) 
  (find-divisor n 2)
)

(define (prime? n)
  ; n is primer if its smallest divisor is itself
  (= n (smallest-divisor n))
)

(prime? 5)
(prime? 4)
