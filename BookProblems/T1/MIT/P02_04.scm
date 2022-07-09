; Exponentiation using a recursive definition
(define (expt b n)
  (if (= n 0)
    ; b^0 = 1
    1
    (* 
      ; b · (b^{n-1}) = b^n
      b 
      ; Call recursive to get b^(n-1)
      (expt b (- n 1))
    )
  )
)

(expt 2 5)
; 32

; Exponentiation using a iterative (lineal) procedure
(define (expt b n)
  ; Call with initial product = 1
  (expt-iter b n 1)
)

(define (expt-iter b counter product)
  (if (= counter 0)
    ; Return the accumulated product until now
    product
    (expt-iter 
      ; Base
      b
      ; Update counter
      (- counter 1)
      ; Update product
      (* b product)
    )
  )
)

(expt 2 5)
; 32

; Exponentiation using 
; b^n = (b^{n/2})^2 if n is even
; b^n = b(b^{n-1}) if n is odd
; This reduces the number os multiplications
; It grows logarithmically, which is better than linearly
; Because n is odd or even and when is odd we compute b^n-1, so n-1 is even
; we will always reduce significantly the number of operations.

(define (fast-expt b n)
  (cond 
    ; b^0 = 1
    ((= n 0) 1)
    ; If even compute b^{n/2} and then square it 
    ((even? n) (square (fast-expt b (/ n 2))))
    ; If odd compute b * (b^{n-1})
    (else (* b (fast-expt b (- n 1))))
  )
)

(fast-expt 2 5)
; 32
