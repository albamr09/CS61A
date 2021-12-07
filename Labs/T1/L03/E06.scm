; Design a procedure that evolves an iterative exponentiation process 
; that uses successive squaring and uses a logarithmic number of steps, as does fast-expt.

; b: base
; n: exponent

(define (fast-expt-iter b n product)
  (cond 
    ((= n 0) product)
    ; If even compute b*b and substract 2 from the exponent
    ((even? n) 
      (fast-expt-iter
        b
        (- n 2)
        (* (square b) product)
      )
    )
    ; If odd compute b * product up to now
    (else 
      (fast-expt-iter
        b
        (- n 1)
        (* b product)
      )
    )
  )
)

(define (fast-expt b n)
  (fast-expt-iter b n 1)
)

(trace fast-expt)

(fast-expt 2 5)
; 32
