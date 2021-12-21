#lang racket
(require berkeley)

; Design a procedure that evolves an iterative exponentiation process 
; that uses successive squaring and uses a logarithmic number of steps, as does fast-expt.

; b: base
; n: exponent
; product: accumulated product

(define (fast-expt-even b n product)
  (if (= n 0) 
    product
    ; Because n is always going to be even if you substract 2 every time
    (fast-expt-even
      b
      ; Substract 2 from the exponent, because we are squaring
      (- n 2)
      ; Compute b*b*accumulated product
      (* (square b) product)
    )
  )
)

; b: base
; n: exponent
(define (fast-expt-iter b n)
  (if (even? n) 
    ; If even use helper method to compute when exponent is even
    (fast-expt-even b n 1)
    ; If odd update n and b in order to use even method
    (fast-expt-even 
      b 
      ; Substract one to n, so now n is even
      (- n 1) 
      ; Set accumulated product to b
      b
    )
  )
)

(trace fast-expt-iter)

(fast-expt-iter 2 5)
; 32
