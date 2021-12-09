; Write a procedure (next-perf n) that tests numbers starting with n and 
; continuing with n+1, n+2, etc. until a perfect number is found. 
; Hint: you’ll need a sum-of-factors subprocedure.

(define (sum-of-factors-helper n i)
  (cond 
    ((= i n) 0.0)
    ((= (remainder n i) 0) (+ i (sum-of-factors-helper n (+ i 1))))
    (else (sum-of-factors-helper n (+ i 1)))
  )
)
(define (sum-of-factors n)
  (sum-of-factors-helper n 1)
)

; Find a perfect number greater than n
(define (next-perf n)
  ; An integer is a perfect number when the sum of all of its factors 
  ; are equal to itself
  (if(=
      (sum-of-factors (+ n 1)) 
      (+ n 1)
    )
    ; If so return n + 1
    (+ n 1)
    ; Else keep searching with n = n + 1
    (next-perf (+ n 1))
  )
)

(next-perf 1)
; 1
(next-perf 6)
; 28
(next-perf 29)
; 496
