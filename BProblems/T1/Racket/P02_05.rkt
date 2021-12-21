#lang racket
(require berkeley)

(provide gcd)

; Compute gcd iteratively with Eucid's Algorithm
; It grows logarithmicaly

(define (gcd a b)
  ; gcd(a,0) = a
  (if (= b 0)
    a 
    ; gcd(a, b) = gcd(b, r), where a = bq + r
    (gcd b (remainder a b))
  )
)

(gcd 4 2)
; 2

